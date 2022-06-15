import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS hiding (ByteString)

import           Test.Hspec                 (describe, errorCall, hspec, it,
                                             shouldBe, shouldThrow)
import           Test.QuickCheck            ()

import           Data.Bits                  (Bits (shiftL, shiftR))
import           GHC.ByteOrder              (ByteOrder (BigEndian, LittleEndian))
import           TiffParser                 (Header (Header), Tiff (TiffFile),
                                             parserTiff, wordsToNum, parseFailTest)

import           Data.Attoparsec.ByteString (parseOnly)




main :: IO ()
main = hspec $ do
  describe "wordsToNum" $ do
    it "works for littleEndian" $ do
      wordsToNum LittleEndian (BS.pack [0x00,0x2A]) `shouldBe` (42 `shiftL` 8 :: Int)

    it "works for bigEndian" $
      wordsToNum BigEndian (BS.pack [0x00,0x2A]) `shouldBe` (42 :: Int)

    it "file Starts Properly" $ do
      parseOnly parserTiff (BS.pack [0x49,0x49]) `shouldBe` Right (TiffFile (Header LittleEndian))
      parseOnly parserTiff (BS.pack [0x4D,0x4D]) `shouldBe` Right (TiffFile (Header BigEndian))
      parseOnly parserTiff (BS.pack [0x00,0x00]) `shouldBe` Left "asdf"

    it "test fail" $ do
      parseOnly parseFailTest BS.empty `shouldBe` Left "asdf"
