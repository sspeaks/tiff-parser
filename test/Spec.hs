import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BS hiding (ByteString)

import           Test.Hspec                 (describe, errorCall, hspec, it,
                                             shouldBe, shouldThrow)
import           Test.QuickCheck            ()

import           Data.Bits                  (Bits (shiftL, shiftR))
import           GHC.ByteOrder              (ByteOrder (BigEndian, LittleEndian))
import           TiffParser                 (Header (Header), Tiff (TiffFile),
                                             parseEndianness, parseHeader,
                                             parseTiff, wordsToNum)

import           Data.Attoparsec.ByteString (parseOnly)
import           Test.Hspec.Attoparsec      (Source ((~>)), shouldFailOn,
                                             shouldParse, shouldSucceedOn)




main :: IO ()
main = do
  baliTiff <- BS.readFile "test/bali.tif"
  pandaTiff <- BS.readFile "test/panda.tif"
  hspec $ do
    describe "wordsToNum" $ do
      it "works for littleEndian" $ do
        wordsToNum LittleEndian (BS.pack [0x00,0x2A]) `shouldBe` (42 `shiftL` 8 :: Int)

      it "works for bigEndian" $
        wordsToNum BigEndian (BS.pack [0x00,0x2A]) `shouldBe` (42 :: Int)

    describe "basic parser tests" $ do
      it "file Starts Properly - little endian" $ do
        BS.pack [0x49,0x49] ~> parseEndianness   `shouldParse` LittleEndian
      it "file Starts Properly - big endian" $ do
        BS.pack [0x4D,0x4D] ~> parseEndianness  `shouldParse` BigEndian
      it "Full header little endian" $ do
        BS.pack [0x49,0x49,42,0]~> parseHeader `shouldParse` Header LittleEndian
      it "Full Header big endian" $ do
        BS.pack [0x4D,0x4D,0,42] ~> parseHeader `shouldParse` Header BigEndian
      it "Full header fail wrong order little endian" $ do
        parseHeader `shouldFailOn`  BS.pack [0x49,0x49,0,42]
      it "Full header fail wrong order big endian" $ do
        parseHeader `shouldFailOn`   BS.pack [0x4D,0x4D,42,0]


      it "file fails on unexpected" $ do
        mapM_ (parseEndianness  `shouldFailOn`) [BS.pack [a, b] |
          a <- [0x00 .. 0xFF],
          b <- [0x00 .. 0xFF],
          not ((a == b && a == 0x49) || (a == b && a == 0x4D))]


    describe "file test" $ do
      it "MM file parses properly" $ do
        parseTiff `shouldSucceedOn` baliTiff
      it "II file parses proeprly" $ do
        parseTiff `shouldSucceedOn` pandaTiff
