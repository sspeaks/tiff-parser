module TiffParser where
import           Data.Bits       (Bits (rotateL, rotateR))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS hiding (ByteString)
import           Data.Word       (Word8)
import           GHC.ByteOrder   (ByteOrder (BigEndian, LittleEndian))

data Tiff = TiffFile Header

-- II = LittleEndian
-- MM = BigEndian
data Header = Header ByteOrder


wordsToNum :: ByteOrder -> ByteString -> Int
wordsToNum bo = BS.foldl f 0
    where f = case bo of
                LittleEndian -> \num w8 -> rotateR num 8 + fromIntegral w8 -- Note ShiftR doesn't Wrap. Need to Address
                BigEndian    -> \num w8 -> rotateL num 8 + fromIntegral w8

bytes :: [Word8]
bytes = [0x2A, 0x00]
