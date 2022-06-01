import qualified Data.ByteString as BS
import           Test.Hspec
import           Test.QuickCheck

import           Data.Bits       (Bits (shiftL, shiftR))
import           GHC.ByteOrder   (ByteOrder (BigEndian, LittleEndian))
import           TiffParser      (wordsToNum)



main :: IO ()
main = hspec $ do
  describe "wordsToNum" $ do
    it "works for littleEndian" $ do
      wordsToNum LittleEndian (BS.pack [0x00,0x2A]) `shouldBe` (42 `shiftL` 8 :: Int)

    it "works for bigEndian" $
      wordsToNum BigEndian (BS.pack [0x00,0x2A]) `shouldBe` (42 :: Int)
