{-# LANGUAGE OverloadedStrings #-}

module TiffParser where

import           Data.Attoparsec.ByteString (Parser, anyWord8, string, take,
                                             try)
import           Data.Attoparsec.Combinator (choice)
import           Data.Bits                  (Bits (rotateL, rotateR, shiftR))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS hiding (ByteString)
import           Data.Functor               (($>))
import           Data.Word                  (Word8)
import           Debug.Trace
import           GHC.ByteOrder              (ByteOrder (BigEndian, LittleEndian))
import           Prelude                    hiding (take)

newtype Tiff = TiffFile Header deriving (Show, Eq)

-- II = LittleEndian
-- MM = BigEndian
newtype Header = Header ByteOrder deriving (Show, Eq)


parserTiff :: Parser Tiff
parserTiff = TiffFile <$> parseHeader

parseHeader :: Parser Header
parseHeader = Header <$> choice [string "MM" $> BigEndian, string "II" $> LittleEndian ]


wordsToNum :: ByteOrder -> ByteString -> Int
wordsToNum bo = res
    where
        bef num w8 = rotateL num 8 + fromIntegral w8
        res = case bo of
                LittleEndian -> BS.foldr (flip bef) 0
                BigEndian    -> BS.foldl bef 0
