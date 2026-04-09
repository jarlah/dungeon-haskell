{-# LANGUAGE DeriveGeneric #-}
-- | Shared save-system types with no dependency on 'GameState'.
--
--   This module exists so 'Game.GameState' can carry a
--   'Game.GameState.SaveMenu' field (for the in-game save/load
--   picker UI) without creating an import cycle with 'Game.Save',
--   which itself imports 'Game.GameState' to serialize it.
--
--   Every type here has a 'Binary' instance so that if it ever ends
--   up transitively reachable from 'GameState', the derivation in
--   'Game.Save' just works.
module Game.Save.Types
  ( SaveSlot(..)
  , SaveError(..)
  , SaveMetadata(..)
  , saveMagic
  ) where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Binary                (Binary)
import           GHC.Generics               (Generic)

-- | Identifies a single save file in the save directory. The game
--   exposes one dedicated 'QuickSlot' driven by the F5/F9 keybinds
--   plus a handful of user-visible 'NumberedSlot's selected from the
--   save menu.
data SaveSlot
  = QuickSlot
  | NumberedSlot !Int
  deriving (Eq, Ord, Show, Generic)

instance Binary SaveSlot

-- | Everything that can go wrong on the save/load path. The game
--   layer never needs to unpack the underlying 'SomeException' — it
--   only cares about the category.
data SaveError
  = SaveMissing
    -- ^ no file at that slot
  | SaveWrongMagic
    -- ^ the file doesn't begin with the 'saveMagic' prefix
  | SaveWrongVersion
    -- ^ magic matched but the version byte sequence doesn't —
    --   the save is from an older (or newer) game build
  | SaveCorrupt !String
    -- ^ header was fine but the 'Data.Binary' decoder rejected
    --   the payload. The string carries the decoder's own message.
  | SaveIOError !String
    -- ^ disk IO raised an 'IOException' (missing dir, permission
    --   denied, ...). The string carries 'show' of the exception.
  deriving (Eq, Show, Generic)

instance Binary SaveError

-- | Small-footprint metadata shown in the save/load menu so the
--   player can tell slots apart without loading them. Extracted by
--   fully decoding the save and reading a handful of fields — it is
--   cheap enough at our save sizes that a dedicated header would be
--   premature optimization.
--
--   The file's modification time is used only internally by
--   'Game.Save.listSaves' to sort most-recent-first; it is
--   deliberately /not/ stored here so this type round-trips through
--   'Binary' cleanly and can live inside 'GameState' as UI state
--   without pulling in 'binary-orphans'.
data SaveMetadata = SaveMetadata
  { smSlot      :: !SaveSlot
  , smDepth     :: !Int
  , smPlayerLvl :: !Int
  , smPlayerHP  :: !Int
  } deriving (Eq, Show, Generic)

instance Binary SaveMetadata

-- | Fixed 8-byte prefix that every save file must begin with. The
--   first six bytes are the format tag, the last two are an ASCII
--   version number. Bumping the format version rejects every save
--   written by an older build of the game.
--
--   Version history:
--
--   * @DHSAVE01@ — Milestones 1–13. Pre-AI schema.
--   * @DHSAVE02@ — Milestone 14. 'NPC' gained the optional
--     'npcAIGreet' cache field, which changes the derived Binary
--     encoding for every 'GameState' that contains an NPC list.
--   * @DHSAVE03@ — Milestone 14 follow-ups. 'DungeonLevel' gained
--     'dlRooms' (so room-entry detection has a stable index), and
--     'GameState' gained 'gsRoomDesc' / 'gsRoomDescVisible' for the
--     dismissable AI room-description panel.
saveMagic :: BL.ByteString
saveMagic = BL8.pack "DHSAVE03"
