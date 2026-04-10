{-# LANGUAGE OverloadedStrings #-}
-- | HTTP client for the LLM backends the game talks to.
--
--   Built on 'Network.HTTP.Tower' so we get retries, timeouts, and a
--   circuit breaker for free. Everything transport-level lives here;
--   the request/response /shape/ (JSON bodies, which endpoint for
--   which provider) is handled inline in 'sendPrompt' and the
--   dedicated per-provider builders.
--
--   Mock mode short-circuits before we ever touch http-tower-hs —
--   'newAIClient' returns a 'MockClient' value that 'sendPrompt'
--   dispatches to "Game.AI.Mock". This keeps the dependency graph
--   linear and lets tests exercise the full AI pipeline without a
--   real network stack.
module Game.AI.Client
  ( AIClient
  , newAIClient
  , closeAIClient
  , sendPrompt
  ) where

import           Control.Exception        (SomeException, try)
import           Data.Aeson               ((.=))
import qualified Data.Aeson               as A
import qualified Data.Aeson.Key           as AK
import qualified Data.Aeson.KeyMap        as AKM
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text                as T
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as TE
import qualified Data.Vector              as V
import           System.Directory         (doesFileExist)
import           System.Environment       (lookupEnv)

import qualified Network.HTTP.Client      as HTTP
import qualified Network.HTTP.Tower       as Tower
import           Network.HTTP.Types.Status (statusCode)

import           Game.AI.Types            (AIError (..))
import qualified Game.AI.Mock             as Mock
import           Game.Config              (AIConfig (..), AIProvider (..))

-- | Lives for the whole game session. Either a real http-tower-hs
--   client with its middleware stack already applied, or a mock
--   placeholder that 'sendPrompt' routes directly to the canned
--   responses module.
--
--   Held opaque on purpose — consumers never reach into the Tower
--   'Tower.Client' directly, which would couple the rest of the
--   codebase to http-tower-hs.
data AIClient
  = RealClient !Tower.Client
  | MockClient
  | DisabledClient

-- | Create an 'AIClient' from the AI config. Decides provider +
--   middleware stack once at startup so per-request code stays
--   fast and allocation-free.
--
--   The middleware stack, composed with '.' in the order a request
--   actually flows through:
--
--   * 'Tower.withRetry' — up to 'aiMaxRetries' attempts with
--     exponential backoff, base 500ms, multiplier 2.
--   * 'Tower.withTimeout' — per-attempt timeout in milliseconds.
--
--   Circuit breaking is intentionally left out of the stack here:
--   'Tower.withCircuitBreaker' needs an externally-managed
--   'Tower.CircuitBreaker' reference, and the extra ceremony isn't
--   worth it for a single-player game. The retry + timeout combo
--   is already enough to prevent a flaky backend from blocking the
--   UI thread for noticeable amounts of time.
newAIClient :: AIConfig -> IO AIClient
newAIClient cfg
  | not (aiEnabled cfg)             = pure DisabledClient
  | aiProvider cfg == ProviderMock  = pure MockClient
  | otherwise = do
      caPath <- findCACert
      base <- Tower.newClientWithTLS caPath Nothing
      let retries = max 0 (aiMaxRetries cfg)
          timeoutMs = max 1 (aiTimeoutSeconds cfg) * 1000
          stack =
              Tower.withRetry (Tower.exponentialBackoff retries 0.5 2.0)
            . Tower.withTimeout timeoutMs
      pure (RealClient (Tower.applyMiddleware stack base))

-- | Release any resources held by a client. The http-client Manager
--   underneath an 'AIClient' is garbage-collected with the client
--   itself, so this is currently a no-op — it exists to give the
--   caller a symmetric start/stop pair and to leave room for future
--   resources (shared background threads, circuit breaker state,
--   ...).
closeAIClient :: AIClient -> IO ()
closeAIClient _ = pure ()

-- | Dispatch a prompt to whichever backend is configured and return
--   either the text reply or an 'AIError'.
--
--   For the real HTTP path this packages the prompt into the
--   provider's expected JSON body, POSTs it, waits (inside the
--   middleware stack) for the response, and extracts the reply
--   text from the JSON. For the mock path this calls into
--   "Game.AI.Mock" directly. For the disabled path this is a
--   constant-time 'Left AIDisabled'.
sendPrompt :: AIClient -> AIConfig -> Text -> IO (Either AIError Text)
sendPrompt DisabledClient _  _      = pure (Left AIDisabled)
sendPrompt MockClient     _  prompt = Right <$> Mock.mockReply prompt
sendPrompt (RealClient c) cfg prompt = do
  eReq <- try (buildRequest cfg prompt) :: IO (Either SomeException HTTP.Request)
  case eReq of
    Left err  ->
      pure (Left (AINetworkError (T.pack ("bad endpoint: " <> show err))))
    Right req -> do
      result <- Tower.runRequest c req
      case result of
        Left svcErr -> pure (Left (fromServiceError svcErr))
        Right resp  ->
          let status = statusCode (HTTP.responseStatus resp)
              body   = HTTP.responseBody resp
          in if status >= 200 && status < 300
               then pure (extractReply cfg body)
               else pure (Left (AIHttpStatus status (T.take 200 (decodeBody body))))

--------------------------------------------------------------------
-- Request / response shape per provider
--------------------------------------------------------------------

-- | Build the raw 'HTTP.Request' for the given prompt. Throws via
--   'HTTP.parseRequest' if the endpoint is malformed — the caller
--   catches that with 'try' so a broken config becomes an 'AIError'
--   instead of a crash.
buildRequest :: AIConfig -> Text -> IO HTTP.Request
buildRequest cfg prompt = do
  base <- HTTP.parseRequest (T.unpack (aiEndpoint cfg))
  let bodyBytes = A.encode (providerBody (aiProvider cfg) (aiModel cfg) prompt)
      authHeaders = case aiProvider cfg of
        ProviderAnthropic
          | not (T.null (aiApiKey cfg)) ->
              [ ("x-api-key",         TE.encodeUtf8 (aiApiKey cfg))
              , ("anthropic-version", "2023-06-01")
              ]
          | otherwise ->
              [ ("anthropic-version", "2023-06-01") ]
        _
          | T.null (aiApiKey cfg) -> []
          | otherwise ->
              [ ( "Authorization"
                , TE.encodeUtf8 ("Bearer " <> aiApiKey cfg)
                )
              ]
  pure base
    { HTTP.method         = "POST"
    , HTTP.requestHeaders =
        [ ("Content-Type", "application/json")
        , ("Accept",       "application/json")
        ] ++ authHeaders
    , HTTP.requestBody    = HTTP.RequestBodyLBS bodyBytes
    }

-- | JSON body shape for each provider. Kept as a single dispatch
--   point so adding a new provider is one case + one extractor.
providerBody :: AIProvider -> Text -> Text -> A.Value
providerBody ProviderMock _ _ =
  -- unreachable: sendPrompt routes mock earlier, but give it a
  -- harmless shape in case the code is ever reshuffled.
  A.object [ "prompt" .= ("" :: Text) ]
providerBody ProviderOllama model prompt = A.object
  [ "model"  .= model
  , "prompt" .= prompt
  , "stream" .= False
  ]
providerBody ProviderOpenAI model prompt = A.object
  [ "model" .= model
  , "messages" .= A.toJSON
      [ A.object [ "role" .= ("user" :: Text), "content" .= prompt ] ]
  ]
providerBody ProviderAnthropic model prompt = A.object
  [ "model"      .= model
  , "max_tokens" .= (1024 :: Int)
  , "messages"   .= A.toJSON
      [ A.object [ "role" .= ("user" :: Text), "content" .= prompt ] ]
  ]

-- | Extract the reply text from a provider response body. Ollama
--   puts the generated text in @response@, OpenAI nests it under
--   @choices[0].message.content@.
extractReply :: AIConfig -> BL.ByteString -> Either AIError Text
extractReply cfg body = case aiProvider cfg of
  ProviderMock   -> Right (decodeBody body)
  ProviderOllama -> case A.eitherDecode body of
    Left err -> Left (AIParseError (T.pack err))
    Right v  -> case v of
      A.Object o -> case lookupKey "response" o of
        Just (A.String t) -> Right t
        _                 -> Left (AIParseError "missing 'response' field")
      _ -> Left (AIParseError "expected JSON object from Ollama")
  ProviderOpenAI -> case A.eitherDecode body of
    Left err -> Left (AIParseError (T.pack err))
    Right v  -> case v of
      A.Object o
        | Just (A.Array choices) <- lookupKey "choices" o
        , Just (A.Object c0)    <- choices V.!? 0
        , Just (A.Object msg) <- lookupKey "message" c0
        , Just (A.String t)   <- lookupKey "content" msg
          -> Right t
      _ -> Left (AIParseError "unexpected OpenAI response shape")
  ProviderAnthropic -> case A.eitherDecode body of
    Left err -> Left (AIParseError (T.pack err))
    Right v  -> case v of
      A.Object o
        | Just (A.Array content) <- lookupKey "content" o
        , Just (A.Object block)  <- content V.!? 0
        , Just (A.String t)      <- lookupKey "text" block
          -> Right t
      _ -> Left (AIParseError "unexpected Anthropic response shape")

-- | Look up a 'Text' key in an aeson 2 'A.Object' (which is a
--   'KeyMap'). Split out so the call sites stay readable.
lookupKey :: Text -> A.Object -> Maybe A.Value
lookupKey k = AKM.lookup (AK.fromText k)

-- | Best-effort UTF-8 decode of a body fragment. Used for building
--   error messages so we never fail /while/ reporting a failure.
decodeBody :: BL.ByteString -> Text
decodeBody = TE.decodeUtf8With (\_ _ -> Just '?') . BL.toStrict

-- | Map a Tower 'Tower.ServiceError' into the narrower 'AIError'
--   the game layer consumes. This is where retries, timeouts, and
--   circuit-breaker trips become game-meaningful failure reasons.
fromServiceError :: Tower.ServiceError -> AIError
fromServiceError err = case err of
  Tower.TransportError e   -> AINetworkError (T.pack (show e))
  Tower.TimeoutError       -> AITimeout
  Tower.RetryExhausted _ e -> AIRetryExhausted (Tower.displayError e)
  Tower.CircuitBreakerOpen -> AICircuitOpen
  Tower.CustomError t      -> AINetworkError t

-- | Find a CA certificate bundle. Checks SSL_CERT_FILE first, then
--   probes well-known system paths. Returns 'Nothing' to fall back
--   to the compiled-in default (which may not work under Nix).
findCACert :: IO (Maybe FilePath)
findCACert = do
  envPath <- lookupEnv "SSL_CERT_FILE"
  case envPath of
    Just p  -> do
      ok <- doesFileExist p
      if ok then pure (Just p) else probe
    Nothing -> probe
  where
    probe = firstExisting
      [ "/etc/ssl/certs/ca-certificates.crt"
      , "/etc/ssl/certs/ca-bundle.crt"
      , "/etc/pki/tls/certs/ca-bundle.crt"
      , "/etc/ssl/ca-bundle.pem"
      ]
    firstExisting []     = pure Nothing
    firstExisting (p:ps) = do
      ok <- doesFileExist p
      if ok then pure (Just p) else firstExisting ps
