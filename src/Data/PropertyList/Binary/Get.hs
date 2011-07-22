module Data.PropertyList.Binary.Get where

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BL
import Data.PropertyList.Binary.Float
import Data.PropertyList.Binary.Types
import Data.Serialize.Get
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time
import qualified Data.Vector.Unboxed as V
import Data.Word

rawBPList bs = do
    let headerBS = BL.take 8 bs
    header@(BPListHeader version) <- runGetLazy bplistHeader headerBS
    when (version .&. 0xff00 /= 0x3000) $
        Left "Unsupported bplist version"
    
    let trailerBS = BL.drop (BL.length bs - bplistTrailerBytes) bs
    trailer <- runGetLazy bplistTrailer trailerBS
    
    --TODO: sanity checks
    let nOffsets :: Num a => a
        nOffsets        = fromIntegral (numObjects trailer)
        bytesPerOffset  = fromIntegral (offsetIntSize trailer)
        offsetsBS
            = BL.take (nOffsets * fromIntegral bytesPerOffset)
            . BL.drop (fromIntegral (offsetTableOffset trailer))
            $ bs
    offsets <- runGetLazy (replicateM nOffsets (sizedInt bytesPerOffset)) offsetsBS
    
    return (RawBPList bs header (V.fromList offsets) trailer)

readBPListRecords :: BL.ByteString -> Either String (BPListRecords Abs)
readBPListRecords bs = do
    raw <- rawBPList bs
    let tlr  = rawTrailer raw
        ct   = numObjects tlr
        root = topObject tlr
    recs <- mapM (getBPListRecord raw) [0 .. ct - 1]
    return (BPListRecords root (Seq.fromList recs))

getBPListRecord (RawBPList bs _hdr offsets tlr) objNum
    | objNum >= 0 && fromIntegral objNum < V.length offsets
    = runGetLazy (bplistRecord objRef) (BL.drop (fromIntegral (offsets V.! fromIntegral objNum)) bs)
    
    | otherwise = Left "getBPListRecord: index out of range"
    where
        objRef = sizedInt (fromIntegral (objectRefSize tlr))

asciiString str = do
    let bs = BSC8.pack str
    bs' <- getByteString (BSC8.length bs)
    if (bs == bs') 
        then return ()
        else fail ("Expecting " ++ show str)

bplistHeaderBytes = 8
bplistHeader = do
    asciiString "bplist"
    BPListHeader <$> getWord16be

bplistTrailerBytes = 32
bplistTrailer =
    const BPListTrailer
        <$> skip 5          -- _unused
        <*> getWord8        -- sortVersion
        <*> getWord8        -- offsetIntSize
        <*> getWord8        -- objectRefSize
        <*> getWord64be     -- numObjects
        <*> getWord64be     -- topObject
        <*> getWord64be     -- offsetTableOffset

bplistRecord ref = msum
    [ const BPLNull     <$> bplNull
    , BPLBool           <$> bplTrue
    , BPLBool           <$> bplFalse
    , const BPLFill     <$> bplFill
    , BPLInt            <$> bplInt
    , BPLReal           <$> bplFloat32
    , BPLReal           <$> bplFloat64
    , BPLDate           <$> bplDate
    , BPLData           <$> bplData
    , BPLString         <$> bplASCII
    , BPLString         <$> bplUTF16
    , BPLUID            <$> bplUID
    , BPLArray          <$> bplArray ref
    , BPLSet            <$> bplSet ref
    , uncurry BPLDict   <$> bplDict ref
    ]

word8 b = do
    b' <- getWord8
    if b == b'
        then return b
        else fail ("expecting " ++ show b)

bplNull  = word8 0x00
bplTrue  = word8 0x08 >> return True
bplFalse = word8 0x09 >> return False
bplFill  = word8 0x0f
bplInt = do
    sz <- shiftL 1 . fromIntegral <$> halfByte 0x1
    i  <- sizedInt sz
    return (interpretBPLInt sz i)
bplFloat32 = do
    word8 0x22
    getFloat32be
bplFloat64 = do
    word8 0x23
    getFloat64be
bplDate = do
    word8 0x33
    interpretBPLDate . word64ToDouble <$> getWord64be
bplData = do
    sz <- markerAndSize 0x4
    getByteString sz
bplASCII = do
    sz <- markerAndSize 0x5
    BSC8.unpack <$> getByteString sz
bplUTF16 = do
    sz <- markerAndSize 0x6
    Text.unpack . Text.decodeUtf16BE <$> getByteString (2*sz)
bplUID = do
    sz <- fmap (1+) (halfByte 0x8)
    sizedInt (fromIntegral sz)
bplArray ref = do
    sz <- markerAndSize 0xA
    replicateM sz ref
bplSet ref = do
    sz <- markerAndSize 0xC
    replicateM sz ref
bplDict ref = do
    sz <- markerAndSize 0xD
    ks <- replicateM sz ref
    vs <- replicateM sz ref
    return (ks, vs)

halfByte x = do
    marker <- getWord8
    if marker `shiftR` 4 == x
        then return (marker .&. 0x0f)
        else fail ("expecting marker " ++ show x)
markerAndSize x = do
    marker <- halfByte x
    case marker of
        0xf -> do
            intSz <- shiftL 1 . fromIntegral <$> halfByte 0x1
            sizedInt intSz
        _   -> return (fromIntegral marker)

sizedInt :: (Integral i, Bits i) => Word -> Get i
sizedInt 0 = return 0
sizedInt 1 = fromIntegral <$> getWord8
sizedInt 2 = fromIntegral <$> getWord16be
sizedInt 4 = fromIntegral <$> getWord32be
sizedInt 8 = fromIntegral <$> getWord64be
sizedInt n
    | n < 0     = fail ("sizedInt: negative size: " ++ show n)
    | otherwise = do
        let a = n `shiftR` 1; b = n - a
        x <- sizedInt a
        y <- sizedInt b
        return ((x `shiftL` (fromIntegral b * 8)) .|. y)

-- CFBinaryPList.c says:
{-
	// in format version '00', 1, 2, and 4-byte integers have to be interpreted as unsigned,
	// whereas 8-byte integers are signed (and 16-byte when available)
	// negative 1, 2, 4-byte integers are always emitted as 8 bytes in format '00'
	// integers are not required to be in the most compact possible representation, but only the last 64 bits are significant currently
-}
interpretBPLInt :: Word -> Integer -> Integer
interpretBPLInt sz i
    | isSigned && testBit i signBit     = i - bit nBits
    | otherwise                         = i
    where
        isSigned = sz >= 8
        nBits = fromIntegral sz * 8
        signBit  = nBits - 1

-- http://developer.apple.com/library/mac/#documentation/CoreFoundation/Reference/CFDateRef/Reference/reference.html 
-- says:
{-
    Absolute time is measured in seconds relative to the absolute reference
    date of Jan 1 2001 00:00:00 GMT. A positive value represents a date
    after the reference date, a negative value represents a date before it.
    For example, the absolute time -32940326 is equivalent to December 16th,
    1999 at 17:54:34.
 -}
interpretBPLDate :: Double -> UTCTime
interpretBPLDate sec = addUTCTime (realToFrac sec) epoch
    where
        epoch = UTCTime (fromGregorian 2001 1 1) 0

getFloat32be :: Get Double
getFloat32be = do
    d <- getWord32be
    return (word32ToDouble d)

getFloat64be :: Get Double
getFloat64be = do
    d <- getWord64be
    return (word64ToDouble d)
