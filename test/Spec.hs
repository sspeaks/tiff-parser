import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS hiding (ByteString)

import           Test.Hspec                 (describe, errorCall, hspec, it,
                                             shouldBe, shouldThrow)
import           Test.QuickCheck            ()

import           Data.Bits                  (Bits (shiftL, shiftR))
import           GHC.ByteOrder              (ByteOrder (BigEndian, LittleEndian))
import           TiffParser                 (Header (Header), Tiff (TiffFile),
                                             parserTiff, wordsToNum)

import           Data.Attoparsec.ByteString (parseOnly)
import           Test.Hspec.Attoparsec      (Source ((~>)), shouldFailOn,
                                             shouldParse)




main :: IO ()
main = hspec $ do
  describe "wordsToNum" $ do
    it "works for littleEndian" $ do
      wordsToNum LittleEndian (BS.pack [0x00,0x2A]) `shouldBe` (42 `shiftL` 8 :: Int)

    it "works for bigEndian" $
      wordsToNum BigEndian (BS.pack [0x00,0x2A]) `shouldBe` (42 :: Int)

  describe "basic parser tests" $ do
    it "file Starts Properly - little endian" $ do
      BS.pack [0x49,0x49] ~> parserTiff  `shouldParse` TiffFile (Header LittleEndian)
    it "file Starts Properly - big endian" $ do
      BS.pack [0x4D,0x4D] ~> parserTiff  `shouldParse` TiffFile (Header BigEndian)
    it "file fails on unexpected" $ do
      mapM_ (parserTiff `shouldFailOn`) [BS.pack [a, b] |
         a <- [0x00 .. 0xFF],
         b <- [0x00 .. 0xFF],
         not ((a == b && a == 0x49) || (a == b && a == 0x4D))]

