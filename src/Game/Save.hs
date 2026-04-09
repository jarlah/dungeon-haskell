{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- ^ Game.Save is deliberately the single home for every Binary
-- instance in the project. Centralizing them here keeps Game.Types
-- / Game.GameState / Game.Logic.* free of serialization concerns,
-- at the cost of every instance being technically orphan. That's
-- an intentional tradeoff, so the warning is suppressed module-wide.
-- | Save-game persistence layer for 'GameState'.
--
--   This module is the single source of truth for how the game is
--   serialized to disk. It provides:
--
--   * orphan 'Generic' / 'Binary' instances for every type reachable
--     from 'GameState' (centralized here so 'Game.Types' and friends
--     stay free of serialization concerns);
--   * a pure encode/decode layer built on 'Data.Binary' with a small
--     magic-header + version prefix so we can reject old or corrupted
--     files cleanly instead of silently decoding nonsense;
--   * a thin IO shell around the filesystem — save directory
--     resolution (via XDG), atomic writes, listing, deletion — that
--     funnels every failure into a single 'SaveError' sum so the game
--     layer can report errors without caring about which library blew
--     up underneath it.
--
--   Design notes:
--
--   * **Binary, not text.** Save files are deliberately opaque so
--     casual "just bump my HP to 999" edits don't work. Anyone with
--     @ghci@ and this module can still decode a save, but that's a
--     much higher bar than opening it in a text editor.
--
--   * **Magic header.** Every save begins with the 8-byte ASCII
--     sequence @DHSAVE\<vv\>@ (Dungeon Haskell SAVE, format version
--     \<vv\>). The current version is @DHSAVE03@. The version trails
--     the magic so that bumping the save format across a schema
--     change is a single-byte patch and old saves get rejected
--     cleanly with 'SaveWrongVersion' instead of silently decoding
--     into garbage. There is no migration path — hobby-project
--     tradeoff.
--
--   * **Atomic writes.** 'writeSave' encodes to a @\<slot\>.save.tmp@
--     tempfile, @hFlush@es, then 'renameFile's over the target. A
--     crash mid-write can corrupt the tempfile but never an existing
--     save.
--
--   * **Graceful degradation.** Every IO boundary is wrapped in @try@
--     and funneled into 'SaveError'. The save system, like the audio
--     system, must never crash the game on a missing file or a
--     permission error — it reports the failure upstream and lets the
--     game keep running.
module Game.Save
  ( -- * Re-exports from "Game.Save.Types"
    SaveSlot(..)
  , SaveError(..)
  , SaveMetadata(..)
  , saveMagic
    -- * Pure encode / decode
  , encodeSave
  , decodeSave
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
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Binary                (Binary (..), decodeOrFail, encode)
import           Data.List                  (sortOn, isSuffixOf, stripPrefix)
import           Data.Ord                   (Down (..))
import           Data.Time.Clock            (UTCTime)
import           Data.Vector.Binary         ()
import           GHC.Generics               (Generic)
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

import           Game.GameState
import           Game.Logic.Quest
import           Game.Save.Types
import           Game.Types
import           System.Random.Internal     (StdGen (..))
import           System.Random.SplitMix     (seedSMGen, unseedSMGen)

--------------------------------------------------------------------
-- Orphan instances
--------------------------------------------------------------------

-- Binary for V2 a is already provided by 'linear' (conditionally
-- behind its Binary instances), so we don't add our own and risk
-- an overlap. Vector/Map/Set come from binary + vector-binary-
-- instances. The only type reachable from 'GameState' that still
-- needs a hand-rolled instance is 'StdGen'.

-- Binary for StdGen: 'random' 1.2+ models 'StdGen' as an opaque
-- newtype around a splitmix 'SMGen', with no 'Read' instance, so
-- a naive Show/Read round-trip doesn't compile. Instead we reach
-- into 'System.Random.Internal' for the newtype constructor and
-- serialize the underlying splitmix state as two 'Word64's via
-- 'unseedSMGen' / 'seedSMGen'. This is 16 bytes, deterministic,
-- and preserves the exact RNG stream so a save/load round-trip
-- is bit-identical with a run that never saved.
instance Binary StdGen where
  put (StdGen smg) =
    let (a, b) = unseedSMGen smg
    in put a >> put b
  get = do
    a <- get
    b <- get
    pure (StdGen (seedSMGen a b))

--------------------------------------------------------------------
-- Generic + Binary for every game type reachable from GameState.
--
-- StandaloneDeriving lets us keep Game.Types / Game.GameState /
-- Game.Logic.Quest / Game.Logic.Dungeon completely free of
-- serialization concerns — every type is stock-derivable because
-- its constructors are exported. Orphan warnings are not enabled
-- in the cabal file so this deliberate centralization is silent.
--------------------------------------------------------------------

deriving instance Generic Dir
deriving instance Generic DoorState
deriving instance Generic Tile
deriving instance Generic DungeonLevel
deriving instance Generic Stats
deriving instance Generic MonsterKind
deriving instance Generic Monster
deriving instance Generic GameEvent
deriving instance Generic Potion
deriving instance Generic Weapon
deriving instance Generic Armor
deriving instance Generic Item
deriving instance Generic Inventory
deriving instance Generic InventoryError
deriving instance Generic GameAction

deriving instance Generic Room

deriving instance Generic QuestGoal
deriving instance Generic QuestStatus
deriving instance Generic QuestEvent
deriving instance Generic Quest

deriving instance Generic NPC
deriving instance Generic ParkedLevel
deriving instance Generic SaveMenu
deriving instance Generic SaveMenuMode
deriving instance Generic SaveMenuEntry
deriving instance Generic LaunchMenu
deriving instance Generic LaunchOption
deriving instance Generic GameState

instance Binary Dir
instance Binary DoorState
instance Binary Tile
instance Binary DungeonLevel
instance Binary Stats
instance Binary MonsterKind
instance Binary Monster
instance Binary GameEvent
instance Binary Potion
instance Binary Weapon
instance Binary Armor
instance Binary Item
instance Binary Inventory
instance Binary InventoryError
instance Binary GameAction

instance Binary Room

instance Binary QuestGoal
instance Binary QuestStatus
instance Binary QuestEvent
instance Binary Quest

instance Binary NPC
instance Binary ParkedLevel
instance Binary SaveMenu
instance Binary SaveMenuMode
instance Binary SaveMenuEntry
instance Binary LaunchMenu
instance Binary LaunchOption
instance Binary GameState

--------------------------------------------------------------------
-- Pure encode / decode
--------------------------------------------------------------------

-- | Serialize a 'GameState' to a lazy bytestring, prefixed with the
--   magic header. Pure — IO happens in 'writeSave'.
encodeSave :: GameState -> BL.ByteString
encodeSave gs = saveMagic <> encode gs

-- | Strict inverse of 'encodeSave'. Verifies the magic header, then
--   runs the binary decoder on the payload. Any decoder failure —
--   short input, trailing garbage, wrong constructor tag — becomes
--   'SaveCorrupt'.
decodeSave :: BL.ByteString -> Either SaveError GameState
decodeSave bs
  | not (saveMagic `BL.isPrefixOf` bs) =
      if BL.length bs < BL.length saveMagic
        -- Very short files can't even fit the header: same user-facing
        -- meaning as "wrong magic" — this isn't one of our files.
        then Left SaveWrongMagic
        else
          -- First six bytes ("DHSAVE") must match; only the version
          -- suffix is allowed to differ on future format bumps.
          let (prefix, _) = BL.splitAt 6 bs
              tag         = BL8.pack "DHSAVE"
          in if prefix == tag
               then Left SaveWrongVersion
               else Left SaveWrongMagic
  | otherwise =
      let payload = BL.drop (BL.length saveMagic) bs
      in case decodeOrFail payload of
           Left (_, _, err)      -> Left (SaveCorrupt err)
           Right (remaining, _, gs)
             | BL.null remaining -> Right gs
             | otherwise         ->
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
saveDir = wrapIO $ do
  base <- getXdgDirectory XdgData "dungeon-haskell"
  let dir = base </> "saves"
  createDirectoryIfMissing True dir
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
slotPath slot = do
  dirRes <- saveDir
  case dirRes of
    Left err  -> pure (Left err)
    Right dir -> pure (Right (dir </> slotFileName slot))

-- | Write a 'GameState' atomically to the given slot. Encodes to a
--   @<path>.tmp@ sibling, @fsync@-ish flushes via 'BL.writeFile',
--   then 'renameFile's over the target. A crash mid-write can
--   corrupt the tempfile but never an existing save.
writeSave :: SaveSlot -> GameState -> IO (Either SaveError ())
writeSave slot gs = do
  pathRes <- slotPath slot
  case pathRes of
    Left err   -> pure (Left err)
    Right path -> wrapIO $ do
      let tmp = path ++ ".tmp"
      BL.writeFile tmp (encodeSave gs)
      renameFile tmp path

-- | Read the save at the given slot. Returns 'SaveMissing' if the
--   file doesn't exist, and otherwise whatever 'decodeSave' returned
--   — including 'SaveWrongMagic' / 'SaveWrongVersion' / 'SaveCorrupt'.
readSave :: SaveSlot -> IO (Either SaveError GameState)
readSave slot = do
  pathRes <- slotPath slot
  case pathRes of
    Left err   -> pure (Left err)
    Right path -> do
      existsRes <- wrapIO (doesFileExist path)
      case existsRes of
        Left err    -> pure (Left err)
        Right False -> pure (Left SaveMissing)
        Right True  -> do
          bsRes <- wrapIO (BL.readFile path)
          case bsRes of
            Left err -> pure (Left err)
            Right bs -> pure (decodeSave bs)

-- | List every known save slot in the save directory, newest first.
--   Unknown or malformed filenames are silently skipped (so the
--   player can drop a @README.txt@ in there without breaking the
--   menu). Each returned metadata is built by fully decoding the
--   save — fine at our sizes, and lets us surface depth/level info
--   in the menu without a separate header file.
listSaves :: IO (Either SaveError [SaveMetadata])
listSaves = do
  dirRes <- saveDir
  case dirRes of
    Left err  -> pure (Left err)
    Right dir -> do
      existsRes <- wrapIO (doesDirectoryExist dir)
      case existsRes of
        Left err    -> pure (Left err)
        Right False -> pure (Right [])
        Right True  -> do
          filesRes <- wrapIO (listDirectory dir)
          case filesRes of
            Left err    -> pure (Left err)
            Right files -> do
              let candidates =
                    [ (s, dir </> f)
                    | f <- files
                    , Just s <- [slotFromFileName (takeFileName f)]
                    ]
              pairs <- mapM readMetaWithMTime candidates
              -- Sort most-recent-first using the file mtime, then
              -- drop the mtime so it doesn't leak into the returned
              -- 'SaveMetadata' (which needs a Binary instance).
              let sorted = sortOn (Down . snd) [p | Just p <- pairs]
              pure (Right (map fst sorted))
  where
    -- Pair each decoded save with its file mtime for sorting.
    -- Saves that fail to decode or stat are silently dropped so a
    -- stray or corrupt file doesn't break the menu.
    readMetaWithMTime
      :: (SaveSlot, FilePath)
      -> IO (Maybe (SaveMetadata, UTCTime))
    readMetaWithMTime (slot, path) = do
      bsRes <- wrapIO (BL.readFile path)
      case bsRes of
        Left _   -> pure Nothing
        Right bs -> case decodeSave bs of
          Left _   -> pure Nothing
          Right gs -> do
            tRes <- wrapIO (getModificationTime path)
            case tRes of
              Left _  -> pure Nothing
              Right t -> pure $ Just
                ( SaveMetadata
                    { smSlot      = slot
                    , smDepth     = dlDepth (gsLevel gs)
                    , smPlayerLvl = sLevel (gsPlayerStats gs)
                    , smPlayerHP  = sHP (gsPlayerStats gs)
                    }
                , t
                )

-- | Delete the save at the given slot. Succeeds silently if the
--   file was already absent — delete is idempotent.
deleteSave :: SaveSlot -> IO (Either SaveError ())
deleteSave slot = do
  pathRes <- slotPath slot
  case pathRes of
    Left err   -> pure (Left err)
    Right path -> do
      existsRes <- wrapIO (doesFileExist path)
      case existsRes of
        Left err    -> pure (Left err)
        Right False -> pure (Right ())
        Right True  -> wrapIO (removeFile path)

--------------------------------------------------------------------
-- IO wrapping helper
--------------------------------------------------------------------

-- | Run an IO action and convert any 'IOException' into a
--   'SaveError'. Used at every filesystem boundary so the caller
--   sees a uniform 'Either SaveError a' regardless of whether the
--   underlying failure was a missing directory, a permission error,
--   or a full disk.
wrapIO :: forall a. IO a -> IO (Either SaveError a)
wrapIO action = do
  (r :: Either IOException a) <- try action
  pure $ case r of
    Left e  -> Left (SaveIOError (show e))
    Right v -> Right v

