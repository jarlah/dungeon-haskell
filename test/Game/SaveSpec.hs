-- | Tests for 'Game.Save'.
--
--   Covers two layers:
--
--   * the pure 'encodeSave' / 'decodeSave' codec — round-trip on a
--     real 'GameState' plus explicit negative tests for the magic
--     header, the version suffix, and payload corruption;
--
--   * the filesystem IO layer ('writeSave', 'readSave', 'listSaves',
--     'deleteSave'). These tests redirect the XDG data directory
--     into a 'withSystemTempDirectory'-managed scratch folder so
--     they can't stomp on the user's real saves.
module Game.SaveSpec (spec) where

import           Control.Exception       (finally)
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Binary             (encode)
import           System.Environment      (lookupEnv, setEnv, unsetEnv)
import           System.IO.Temp          (withSystemTempDirectory)
import           Test.Hspec

import           Game.GameState          (GameState(..), hardcodedInitialState)
import           Game.Save

spec :: Spec
spec = do

  describe "Game.Save.encodeSave / decodeSave" $ do

    it "round-trips a GameState: player position is preserved" $ do
      let gs = hardcodedInitialState
      case decodeSave (encodeSave gs) of
        Right gs' -> gsPlayerPos gs' `shouldBe` gsPlayerPos gs
        Left e    -> expectationFailure ("decode failed: " ++ show e)

    it "round-trips a GameState: messages are preserved" $ do
      let gs = hardcodedInitialState
      case decodeSave (encodeSave gs) of
        Right gs' -> gsMessages gs' `shouldBe` gsMessages gs
        Left e    -> expectationFailure ("decode failed: " ++ show e)

    it "rejects a buffer that doesn't start with the magic header" $ do
      let bad = BL8.pack "not-a-dungeon-haskell-save"
      expectLeft SaveWrongMagic (decodeSave bad)

    it "rejects a buffer shorter than the magic header" $
      expectLeft SaveWrongMagic (decodeSave (BL8.pack "DH"))

    it "rejects a buffer with a DHSAVE prefix but an unknown version" $ do
      -- Right family ("DHSAVE"), wrong version suffix ("99") — this
      -- is the "saved with a newer/older game" case and must come
      -- back as SaveWrongVersion, not SaveWrongMagic.
      let bad = BL8.pack "DHSAVE99" <> encode hardcodedInitialState
      expectLeft SaveWrongVersion (decodeSave bad)

    it "rejects a buffer with a valid header but garbage payload" $ do
      let bad = saveMagic <> BL8.pack "not a real binary encoding"
      case decodeSave bad of
        Left (SaveCorrupt _) -> pure ()
        other ->
          expectationFailure
            ("expected SaveCorrupt, got: " ++ show other)

    it "rejects a valid encoding with trailing junk bytes" $ do
      let junk = encodeSave hardcodedInitialState <> BL8.pack "xxx"
      case decodeSave junk of
        Left (SaveCorrupt _) -> pure ()
        other ->
          expectationFailure
            ("expected SaveCorrupt, got: " ++ show other)

    it "saveMagic is exactly DHSAVE02" $
      saveMagic `shouldBe` BL8.pack "DHSAVE02"

  describe "slotFileName / slotFromFileName" $ do

    it "round-trips the quicksave slot" $
      slotFromFileName (slotFileName QuickSlot) `shouldBe` Just QuickSlot

    it "round-trips a numbered slot" $
      slotFromFileName (slotFileName (NumberedSlot 3))
        `shouldBe` Just (NumberedSlot 3)

    it "rejects stray filenames in the save directory" $ do
      slotFromFileName "README.txt"  `shouldBe` Nothing
      slotFromFileName "slot-.save"  `shouldBe` Nothing
      slotFromFileName "slot-ab.save" `shouldBe` Nothing

  describe "Game.Save filesystem IO" $ around_ withIsolatedSaveDir $ do

    it "writeSave + readSave: round-trips through the disk" $ do
      let gs = hardcodedInitialState
      wRes <- writeSave QuickSlot gs
      expectRightUnit wRes
      rRes <- readSave QuickSlot
      case rRes of
        Right gs' -> gsPlayerPos gs' `shouldBe` gsPlayerPos gs
        Left e    -> expectationFailure ("read failed: " ++ show e)

    it "readSave returns SaveMissing when the slot is empty" $ do
      rRes <- readSave (NumberedSlot 4)
      expectLeft SaveMissing rRes

    it "listSaves lists every written slot" $ do
      _ <- writeSave (NumberedSlot 1) hardcodedInitialState
      _ <- writeSave (NumberedSlot 2) hardcodedInitialState
      _ <- writeSave QuickSlot        hardcodedInitialState
      res <- listSaves
      case res of
        Right ms -> length ms `shouldBe` 3
        Left e   -> expectationFailure ("listSaves failed: " ++ show e)

    it "listSaves skips stray files it doesn't recognize" $ do
      _ <- writeSave QuickSlot hardcodedInitialState
      -- Drop a junk file into the save dir. Listing must still
      -- succeed and not count the junk file as a save.
      dirRes <- saveDir
      case dirRes of
        Left e -> expectationFailure ("saveDir failed: " ++ show e)
        Right dir -> do
          BL.writeFile (dir ++ "/README.txt")
            (BL8.pack "not a save file")
          res <- listSaves
          case res of
            Right ms -> length ms `shouldBe` 1
            Left e   -> expectationFailure ("listSaves failed: " ++ show e)

    it "listSaves drops corrupted save files instead of crashing" $ do
      _ <- writeSave (NumberedSlot 1) hardcodedInitialState
      -- Overwrite a valid save with garbage — decoding will fail
      -- and the entry must be silently dropped from the listing.
      dirRes <- saveDir
      case dirRes of
        Left e -> expectationFailure ("saveDir failed: " ++ show e)
        Right dir -> do
          BL.writeFile (dir ++ "/slot-2.save")
            (BL8.pack "definitely-not-a-save")
          res <- listSaves
          case res of
            Right ms -> length ms `shouldBe` 1
            Left e   -> expectationFailure ("listSaves failed: " ++ show e)

    it "deleteSave is idempotent on empty slots" $ do
      r <- deleteSave (NumberedSlot 5)
      expectRightUnit r

    it "deleteSave removes a written save" $ do
      _    <- writeSave QuickSlot hardcodedInitialState
      dRes <- deleteSave QuickSlot
      expectRightUnit dRes
      rRes <- readSave QuickSlot
      expectLeft SaveMissing rRes

-- | 'shouldBe' on @Either SaveError GameState@ doesn't work because
--   'GameState' has no 'Eq' instance. This helper pattern-matches on
--   the expected error tag and fails explicitly with the actual
--   'Left' value on mismatch.
expectLeft :: SaveError -> Either SaveError a -> Expectation
expectLeft expected result = case result of
  Left actual
    | actual == expected -> pure ()
    | otherwise          ->
        expectationFailure
          ("expected Left " ++ show expected
           ++ ", got Left " ++ show actual)
  Right _ ->
    expectationFailure
      ("expected Left " ++ show expected ++ ", got Right _")

-- | Assert @Either SaveError ()@ is @Right ()@. Same reason as
--   'expectLeft': 'Right ()' is comparable by 'shouldBe' but keeping
--   the two helpers in one place makes the call sites symmetric.
expectRightUnit :: Either SaveError () -> Expectation
expectRightUnit (Right ()) = pure ()
expectRightUnit (Left e)   =
  expectationFailure ("expected Right (), got Left " ++ show e)

-- | Run an IO action with @XDG_DATA_HOME@ pointed at a throwaway
--   temporary directory, so filesystem tests can exercise
--   'writeSave' / 'readSave' / 'listSaves' without touching the
--   user's real save folder. The old value of @XDG_DATA_HOME@ is
--   restored on exit, even if the test throws.
withIsolatedSaveDir :: IO () -> IO ()
withIsolatedSaveDir action = do
  prev <- lookupEnv "XDG_DATA_HOME"
  withSystemTempDirectory "dh-save-test" $ \tmp -> do
    setEnv "XDG_DATA_HOME" tmp
    action `finally` restore prev
  where
    restore Nothing    = unsetEnv "XDG_DATA_HOME"
    restore (Just old) = setEnv "XDG_DATA_HOME" old
