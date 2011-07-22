module Data.PropertyList.Binary.Put where

import Control.Monad
import Data.Serialize.Put
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.Char
import Data.Foldable (toList)
import Data.PropertyList.Binary.Float
import Data.PropertyList.Binary.Types
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time
import Data.Word

withSize putThing = do
    let thing = runPut putThing
    putByteString thing
    return $! (fromIntegral (BS.length thing) :: Word64)

unsnoc []     = error "unsnoc: empty list"
unsnoc [x]    = ([], x)
unsnoc (x:xs) = let ~(ys, y) = unsnoc xs in (x:ys, y)

putBPList (BPListRecords root recs) = do
    let header = bplist00hdr
        nObjs  = Seq.length recs
        objRefSz = unsignedSz nObjs
        putObjRef = putSizedInt objRefSz
    
    putBPListHeader header
    recSizes <- mapM (withSize . putBPListRecord putObjRef) (toList recs)
    
    let (offsets, offsetTblLoc) = unsnoc (scanl (+) 8 recSizes)
        offsetSz        = unsignedSz (offsetTblLoc)
        putOffset       = putSizedInt (fromIntegral offsetSz)
        
        trailer = BPListTrailer 
            { sortVersion       = 0
            , offsetIntSize     = fromIntegral offsetSz
            , objectRefSize     = fromIntegral objRefSz
            , numObjects        = fromIntegral nObjs
            , topObject         = root
            , offsetTableOffset = offsetTblLoc
            }
    
    mapM_ putOffset offsets
    putBPListTrailer trailer


putBPListHeader (BPListHeader v) = do
    putByteString (BSC8.pack "bplist")
    putWord16be v

putBPListTrailer tlr = do
    replicateM 5 (putWord8 0x00)
    putWord8    (sortVersion       tlr)
    putWord8    (offsetIntSize     tlr)
    putWord8    (objectRefSize     tlr)
    putWord64be (numObjects        tlr)
    putWord64be (topObject         tlr)
    putWord64be (offsetTableOffset tlr)

putBPListRecord putRef BPLNull             = putWord8 0x00
putBPListRecord putRef BPLFill             = putWord8 0x0f
putBPListRecord putRef (BPLArray     xs)   = do
    putMarkerWithSize 0xA (length xs)
    mapM_ putRef xs
putBPListRecord putRef (BPLSet       xs)   = do
    putMarkerWithSize 0xC (length xs)
    mapM_ putRef xs
putBPListRecord putRef (BPLData       x)   = do
    putMarkerWithSize 0x4 (BS.length x)
    putByteString x
putBPListRecord putRef (BPLDate       x)   = do
    putWord8 0x33
    putBPLDate x
putBPListRecord putRef (BPLDict   ks vs) 
    | nks /= nvs    = fail "putBPListRecord: BPLDict has different number of keys and values"
    | otherwise     = do
        putMarkerWithSize 0xD nks
        mapM_ putRef ks
        mapM_ putRef vs
    where nks = length ks; nvs = length vs
putBPListRecord putRef (BPLReal       x)   = do
    case doubleToEquivalentFloat x of
        Just f -> do
            putWord8 0x22
            putFloat32be f
        Nothing -> do
            putWord8 0x23
            putFloat64be x
putBPListRecord putRef (BPLInt        x)   = putInt x
putBPListRecord putRef (BPLString     x)   = putString x
putBPListRecord putRef (BPLUID        x)   = putUID x
putBPListRecord putRef (BPLBool   False)   = putWord8 0x08
putBPListRecord putRef (BPLBool    True)   = putWord8 0x09

putMarkerWithPayload x payload = putWord8 ((x `shiftL` 4) .|. payload)
putMarkerWithSize x sz
    | sz < 0x0f = do
        putMarkerWithPayload x (fromIntegral sz)
    | otherwise = do
        putMarkerWithPayload x 0x0f
        putInt (fromIntegral sz)

putInt n
    | tag < 0       = fail "putInt: internal error - size is negative"
    | tag <= 0xf    = do
        putMarkerWithPayload 0x1 (fromIntegral tag)
        putSizedInt nBytes n
    | otherwise     = fail "putInt: Integer too large to encode in a bplist00"
    where (tag, nBytes) = plIntSz n

putSizedInt 0 _ = return ()
putSizedInt 1 i = putWord8    (fromIntegral i)
putSizedInt 2 i = putWord16be (fromIntegral i)
putSizedInt 4 i = putWord32be (fromIntegral i)
putSizedInt 8 i = putWord64be (fromIntegral i)
putSizedInt n i
    | n < 0     = fail "putSizedInt: size is negative"
    | otherwise = do
        let a = n `shiftR` 1; b = n - a
        putSizedInt a (shiftR i (shiftL b 3))
        putSizedInt b i

-- tag and power-of-two  number of bytes needed to represent 'n' 
-- as an int.  If the type is bounded, then this logic works for 
-- negative numbers as well (works for positive but not negative 
-- 'Integer's)
wordLgSz n = go 0 8 0xff
    where
        go lgSz nBits mask
            | n .&. mask == n   = (lgSz, shiftR nBits 3)
            | otherwise         = ((go $! lgSz+1) $! shiftL nBits 1) $! (shiftL mask nBits .|. mask)

-- tag and power-of-two number of bytes needed to represent 'n' as a
-- 2s-complement signed int
intLgSz n
    | n >= 0    = wordLgSz (2 * n)
    | otherwise = wordLgSz (2 * negate (n+1))

-- tag and number of bytes needed to represent 'n' as a bplist00 int, 
-- which is has 2^tag bytes and is signed iff it has 8 or more bytes.
plIntSz :: Integer -> (Int, Int)
plIntSz n
    | n < 0      = max (3,8) (intLgSz n)
    | n < bit 63 = wordLgSz n
    | otherwise  = intLgSz n

putBPLDate utcDate = putFloat64be (realToFrac (diffUTCTime utcDate epoch))
    where
        epoch = UTCTime (fromGregorian 2001 1 1) 0

putString str
    | all isAscii str   = do
        putMarkerWithSize 0x5 (length str)
        putByteString (BSC8.pack str)
    | otherwise         = do
        let utf16 = Text.encodeUtf16BE (Text.pack str)
        putMarkerWithSize 0x6 (BS.length utf16 `shiftR` 1)
        putByteString utf16


putUID i 
    | sz > maxNBytes    = fail ("putUID: UID is too large (it would require " ++ show sz ++ " bytes to encode, but the bplist00 format only supports " ++ show maxNBytes ++ ")")
    | otherwise = do
        putMarkerWithSize 0x8 (sz-1)
        putSizedInt sz i
    where
        sz = unsignedSz i
        maxNBytes = 16

-- return the number of bytes required to represent an unsigned value.  Always returns at least 1.
unsignedSz n = go 1 0xff
    where
        go nBytes mask
            | n .&. mask == n   = nBytes
            | otherwise         = (go $! (nBytes + 1)) $! (shiftL mask 8 .|. mask)

putFloat32be x = putWord32be $! floatToWord32  x
putFloat64be x = putWord64be $! doubleToWord64 x
