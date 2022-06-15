{-# LANGUAGE OverloadedStrings #-}

module TiffParser where

import           Data.Attoparsec.ByteString (Parser, anyWord8, string, take,
                                             try)
import           Data.Attoparsec.Combinator (choice)
import           Data.Binary                (Word16)
import           Data.Binary.Put            (putWord16be, putWord16le, runPut)
import           Data.Bits                  (Bits (rotateL, rotateR, shiftR))
import           Data.ByteString.Lazy       (ByteString, toStrict)
import qualified Data.ByteString.Lazy       as BS hiding (ByteString)
import           Data.Functor               (($>))
import           Data.Word                  (Word8)
import           Debug.Trace                ()
import           GHC.ByteOrder              (ByteOrder (BigEndian, LittleEndian))
import           Prelude                    hiding (take)

newtype Tiff = TiffFile Header deriving (Show, Eq)

-- II = LittleEndian
-- MM = BigEndian
newtype Header = Header ByteOrder deriving (Show, Eq)


parserTiff :: Parser Tiff
parserTiff = do
    header <- parseHeader
    let getNumToParse = let (Header bo) = header in numToWords16 bo
    string . toStrict $ getNumToParse 42
    return $ TiffFile header

parseHeader :: Parser Header
parseHeader = Header <$> choice [string "MM" $> BigEndian, string "II" $> LittleEndian ]

numToWords16 :: ByteOrder -> Word16  -> ByteString
numToWords16 bo num
    | bo == LittleEndian = runPut $ putWord16le num
    | otherwise = runPut $ putWord16be num



wordsToNum :: ByteOrder -> ByteString -> Int
wordsToNum bo = res
    where
        bef num w8 = rotateL num 8 + fromIntegral w8
        res = case bo of
                LittleEndian -> BS.foldr (flip bef) 0
                BigEndian    -> BS.foldl bef 0
