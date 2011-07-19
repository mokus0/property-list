module Data.PropertyList.Binary.Parse where

import Control.Applicative
import Control.Monad
import Data.Attoparsec as Atto
import Data.Attoparsec.Binary as Atto
import Data.Binary.IEEE754
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.PropertyList.Binary.Types
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time
import qualified Data.Vector.Unboxed as V
import Data.Word
import GHC.Float

rawBPList bs = do
    let headerBS = BS.take 8 bs
    header <- parseOnly bplistHeader headerBS
    
    let trailerBS = BS.drop (BS.length bs - bplistTrailerBytes) bs
    trailer <- parseOnly bplistTrailer trailerBS
    
    --TODO: sanity checks
    let nOffsets        = fromIntegral (numObjects trailer)
        bytesPerOffset  = fromIntegral (offsetIntSize trailer)
        offsetsBS
            = BS.take (nOffsets * fromIntegral bytesPerOffset)
            . BS.drop (fromIntegral (offsetTableOffset trailer))
            $ bs
    offsets <- parseOnly (replicateM nOffsets (sizedInt bytesPerOffset)) offsetsBS
    
    return (RawBPList bs header (V.fromList offsets) trailer)

readBPListRecords bs = do
    raw <- rawBPList bs
    let tlr  = rawTrailer raw
        ct   = numObjects tlr
        root = topObject tlr
    recs <- mapM (getBPListRecord raw) [0 .. ct - 1]
    return (BPListRecords root (Seq.fromList recs))

getBPListRecord (RawBPList bs _hdr offsets tlr) objNum
    | objNum >= 0 && fromIntegral objNum < V.length offsets
    = parseOnly (bplistRecord objRef) (BS.drop (fromIntegral (offsets V.! fromIntegral objNum)) bs)
    
    | otherwise = fail "getBPListRecord: index out of range"
    where
        objRef = sizedInt (fromIntegral (objectRefSize tlr))

bplistHeaderBytes = 8
bplistHeader = do
    string (BSC8.pack "bplist")
    BPListHeader <$> anyWord16be

bplistTrailerBytes = 32
bplistTrailer =
    const BPListTrailer
        <$> Atto.take 5     -- _unused
        <*> anyWord8        -- sortVersion
        <*> anyWord8        -- offsetIntSize
        <*> anyWord8        -- objectRefSize
        <*> anyWord64be     -- numObjects
        <*> anyWord64be     -- topObject
        <*> anyWord64be     -- offsetTableOffset

bplistRecord ref = choice
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
    float2Double . wordToFloat <$> anyWord32be
bplFloat64 = do
    word8 0x23
    wordToDouble <$> anyWord64be
bplDate = do
    word8 0x33
    interpretBPLDate . wordToDouble <$> anyWord64be
bplData = do
    sz <- markerAndSize 0x4
    Atto.take sz
bplASCII = do
    sz <- markerAndSize 0x5
    BSC8.unpack <$> Atto.take sz
bplUTF16 = do
    sz <- markerAndSize 0x6
    Text.unpack . Text.decodeUtf16BE <$> Atto.take (2*sz)
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
    marker <- satisfy $ \x' -> x' `shiftR` 4 == x
    return (marker .&. 0x0f)
markerAndSize x = do
    marker <- halfByte x
    case marker of
        0xf -> do
            intSz <- shiftL 1 . fromIntegral <$> halfByte 0x1
            sizedInt intSz
        _   -> return (fromIntegral marker)

sizedInt :: (Integral i, Bits i) => Word -> Parser i
sizedInt 0 = return 0
sizedInt 1 = fromIntegral <$> anyWord8
sizedInt 2 = fromIntegral <$> anyWord16be
sizedInt 4 = fromIntegral <$> anyWord32be
sizedInt 8 = fromIntegral <$> anyWord64be
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
