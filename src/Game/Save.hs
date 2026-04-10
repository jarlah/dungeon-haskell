{-# LANGUAGE ScopedTypeVariables #-}
-- | Save-game persistence layer for 'GameState'.
--
--   This module provides:
--
--   * a pure encode/decode layer built on 'Data.Binary' with a small
--     magic-header + version prefix so we can reject old or corrupted
--     files cleanly instead of silently decoding nonsense;
--   * a thin IO shell around the filesystem — save directory
--     resolution (via XDG), atomic writes, listing, deletion — that
--     funnels every failure into a single 'SaveError' sum so the game
--     layer can report errors without caring about which library blew
--     up underneath it.
--
--   Orphan 'Generic' / 'Binary' instances for every type reachable
--   from 'GameState' live in "Game.Save.Binary" and are imported
--   here for their side-effects.
module Game.Save
  ( -- * Re-exports from "Game.Save.Types"
    SaveSlot(..)
  , SaveError(..)
  , SaveMetadata(..)
  , saveMagic
    -- * Pure encode / decode
  , encodeSave
  , decodeSave
  , decodeHeader
    -- * Filesystem IO
  , saveDir
  , slotPath
  , slotFileName
  , writeSave
  , readSave
  , listSaves
  , deleteSave
    -- * Re-exported for tests
  , slotFromFileName
  ) where

import           Control.Exception          (IOException, try)
import           Control.Monad              (when)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Binary                (decodeOrFail, encode)
import           Data.List                  (sortOn, isSuffixOf, stripPrefix)
import           Data.Maybe                 (catMaybes)
import           Data.Ord                   (Down (..))
import           Data.Time.Clock            (UTCTime)
import           System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getModificationTime
  , getXdgDirectory
  , listDirectory
  , removeFile
  , renameFile
  , XdgDirectory (..)
  )
import           System.FilePath            ((</>), (<.>), takeFileName)

import           Game.Core
import           Game.Save.Binary           ()
import           Game.Save.Types
import           Game.Types

--------------------------------------------------------------------
-- Pure encode / decode
--------------------------------------------------------------------

-- | Build a 'SaveHeader' from the current 'GameState'.
mkHeader :: GameState -> SaveHeader
mkHeader gs = SaveHeader
  { shDepth      = dlDepth (gsLevel gs)
  , shPlayerLvl  = sLevel (gsPlayerStats gs)
  , shPlayerHP   = sHP (gsPlayerStats gs)
  , shCheatsUsed = gsCheatsUsed gs
  }

-- | Serialize a 'GameState' to a lazy bytestring, prefixed with the
--   magic header and a 'SaveHeader'. Pure — IO happens in 'writeSave'.
encodeSave :: GameState -> BL.ByteString
encodeSave gs = saveMagic <> encode (mkHeader gs) <> encode gs

-- | Validate the magic prefix common to both decode paths.
checkMagic :: BL.ByteString -> Either SaveError BL.ByteString
checkMagic bs
  | saveMagic `BL.isPrefixOf` bs = Right (BL.drop (BL.length saveMagic) bs)
  | BL.length bs < BL.length saveMagic = Left SaveWrongMagic
  | BL8.pack "DHSAVE" `BL.isPrefixOf` bs = Left SaveWrongVersion
  | otherwise = Left SaveWrongMagic

-- | Decode only the 'SaveHeader' from a save file, ignoring the
--   'GameState' payload entirely. Used by 'listSaves' to populate
--   the save menu cheaply.
decodeHeader :: BL.ByteString -> Either SaveError SaveHeader
decodeHeader bs = do
  afterMagic <- checkMagic bs
  case decodeOrFail afterMagic of
    Left  (_, _, err) -> Left (SaveCorrupt err)
    Right (_, _, hdr) -> Right hdr

-- | Strict inverse of 'encodeSave'. Verifies the magic prefix,
--   skips the 'SaveHeader', then decodes the 'GameState' payload.
decodeSave :: BL.ByteString -> Either SaveError GameState
decodeSave bs = do
  afterMagic <- checkMagic bs
  case decodeOrFail afterMagic of
    Left  (_, _, err)          -> Left (SaveCorrupt err)
    Right (afterHeader, _, _h :: SaveHeader) ->
      case decodeOrFail afterHeader of
        Left  (_, _, err)      -> Left (SaveCorrupt err)
        Right (remaining, _, gs)
          | BL.null remaining  -> Right gs
          | otherwise          ->
              Left (SaveCorrupt "trailing bytes after GameState")

--------------------------------------------------------------------
-- Filesystem IO
--------------------------------------------------------------------

-- | Resolve the directory that holds save files for this user, and
--   create it on demand. Uses the XDG data directory by default
--   (@$XDG_DATA_HOME/dungeon-haskell/saves@ on Linux,
--   @~/Library/Application Support/dungeon-haskell/saves@ on macOS,
--   @%APPDATA%\\dungeon-haskell\\saves@ on Windows).
saveDir :: IO (Either SaveError FilePath)
saveDir = runSaveIO $ do
  base <- tryIO $ getXdgDirectory XdgData "dungeon-haskell"
  let dir = base </> "saves"
  tryIO $ createDirectoryIfMissing True dir
  pure dir

-- | Filename (without directory) for a save slot. Kept as a pure
--   function so tests can round-trip slot identity through a name
--   without touching the disk.
slotFileName :: SaveSlot -> FilePath
slotFileName QuickSlot        = "quicksave.save"
slotFileName (NumberedSlot n) = "slot-" ++ show n <.> "save"

-- | Inverse of 'slotFileName'. Returns 'Nothing' for filenames that
--   don't match a known slot pattern (so stray files in the save dir
--   don't break 'listSaves').
slotFromFileName :: FilePath -> Maybe SaveSlot
slotFromFileName fn
  | fn == "quicksave.save" = Just QuickSlot
  | Just rest <- stripPrefix "slot-" fn
  , ".save" `isSuffixOf` rest
  , let numStr = take (length rest - length (".save" :: String)) rest
  , not (null numStr)
  , all (`elem` ("0123456789" :: String)) numStr
  = Just (NumberedSlot (read numStr))
  | otherwise = Nothing

-- | Absolute path for a save slot. Creates the save directory on
--   demand — callers don't have to worry about it existing.
slotPath :: SaveSlot -> IO (Either SaveError FilePath)
slotPath slot = runSaveIO $ do
  dir <- saveIO saveDir
  pure (dir </> slotFileName slot)

-- | Write a 'GameState' atomically to the given slot. Encodes to a
--   @<path>.tmp@ sibling, @fsync@-ish flushes via 'BL.writeFile',
--   then 'renameFile's over the target. A crash mid-write can
--   corrupt the tempfile but never an existing save.
writeSave :: SaveSlot -> GameState -> IO (Either SaveError ())
writeSave slot gs = runSaveIO $ do
  path <- saveIO (slotPath slot)
  let tmp = path ++ ".tmp"
  tryIO $ BL.writeFile tmp (encodeSave gs)
  tryIO $ renameFile tmp path

-- | Read the save at the given slot. Returns 'SaveMissing' if the
--   file doesn't exist, and otherwise whatever 'decodeSave' returned
--   — including 'SaveWrongMagic' / 'SaveWrongVersion' / 'SaveCorrupt'.
readSave :: SaveSlot -> IO (Either SaveError GameState)
readSave slot = runSaveIO $ do
  path   <- saveIO (slotPath slot)
  exists <- tryIO (doesFileExist path)
  if not exists
    then throwE SaveMissing
    else do
      bs <- tryIO (BL.readFile path)
      liftEither (decodeSave bs)

-- | List every known save slot in the save directory, newest first.
--   Unknown or malformed filenames are silently skipped (so the
--   player can drop a @README.txt@ in there without breaking the
--   menu). Metadata is read from the fixed 'SaveHeader' without
--   decoding the full 'GameState'.
listSaves :: IO (Either SaveError [SaveMetadata])
listSaves = runSaveIO $ do
  dir    <- saveIO saveDir
  exists <- tryIO (doesDirectoryExist dir)
  if not exists
    then pure []
    else do
      files <- tryIO (listDirectory dir)
      let candidates =
            [ (s, dir </> f)
            | f <- files
            , Just s <- [slotFromFileName (takeFileName f)]
            ]
      pairs <- liftIO $ mapM readMetaWithMTime candidates
      let sorted = sortOn (Down . snd) (catMaybes pairs)
      pure (map fst sorted)
  where
    -- Pair each save's header with its file mtime for sorting.
    -- Saves that fail to decode or stat are silently dropped so a
    -- stray or corrupt file doesn't break the menu.
    readMetaWithMTime
      :: (SaveSlot, FilePath)
      -> IO (Maybe (SaveMetadata, UTCTime))
    readMetaWithMTime (slot, path) = do
      r <- runSaveIO $ do
        bs <- tryIO (BL.readFile path)
        hdr <- liftEither (decodeHeader bs)
        t   <- tryIO (getModificationTime path)
        pure ( SaveMetadata
                 { smSlot       = slot
                 , smDepth      = shDepth hdr
                 , smPlayerLvl  = shPlayerLvl hdr
                 , smPlayerHP   = shPlayerHP hdr
                 , smCheatsUsed = shCheatsUsed hdr
                 }
             , t
             )
      pure $ either (const Nothing) Just r

-- | Delete the save at the given slot. Succeeds silently if the
--   file was already absent — delete is idempotent.
deleteSave :: SaveSlot -> IO (Either SaveError ())
deleteSave slot = runSaveIO $ do
  path   <- saveIO (slotPath slot)
  exists <- tryIO (doesFileExist path)
  when exists $ tryIO (removeFile path)

--------------------------------------------------------------------
-- ExceptT helpers
--------------------------------------------------------------------

type SaveIO = ExceptT SaveError IO

-- | Run a 'SaveIO' computation, returning the result as
--   @IO (Either SaveError a)@ — the public API boundary.
runSaveIO :: SaveIO a -> IO (Either SaveError a)
runSaveIO = runExceptT

-- | Lift an existing @IO (Either SaveError a)@ into 'SaveIO'.
saveIO :: IO (Either SaveError a) -> SaveIO a
saveIO = ExceptT

-- | Run a raw IO action, catching any 'IOException' as 'SaveIOError'.
tryIO :: IO a -> SaveIO a
tryIO action = ExceptT $ do
  (r :: Either IOException a) <- try action
  pure $ case r of
    Left e  -> Left (SaveIOError (show e))
    Right v -> Right v

-- | Lift a pure 'Either SaveError a' into 'SaveIO'.
liftEither :: Either SaveError a -> SaveIO a
liftEither = ExceptT . pure

