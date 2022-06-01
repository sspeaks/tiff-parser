module TiffParser where

import           Data.Bits       (Bits (rotateL, rotateR, shiftR))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS hiding (ByteString)
import           Data.Word       (Word8)
import           GHC.ByteOrder   (ByteOrder (BigEndian, LittleEndian))

data Tiff = TiffFile Header

-- II = LittleEndian
-- MM = BigEndian
data Header = Header ByteOrder


wordsToNum :: ByteOrder -> ByteString -> Int
wordsToNum bo = res
    where
        bef num w8 = rotateL num 8 + fromIntegral w8
        res = case bo of
                LittleEndian -> BS.foldr (flip bef) 0
                BigEndian    -> BS.foldl bef 0

bytes :: [Word8]
bytes = [0x00,0x2A]
