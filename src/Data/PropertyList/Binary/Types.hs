{-# LANGUAGE EmptyDataDecls #-}
module Data.PropertyList.Binary.Types where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Time (UTCTime)
import Data.Vector.Unboxed (Vector)
import Data.Word

data BPListRecord
    = BPLNull
    | BPLFill
    | BPLArray [Word64]
    | BPLSet [Word64]
    | BPLData ByteString
    | BPLDate UTCTime
    | BPLDict [Word64] [Word64]
    | BPLReal Double
    | BPLInt Integer
    | BPLString String
    | BPLUID Integer
    | BPLBool Bool
    deriving (Eq, Ord, Show)

mapObjRefs f (BPLArray   xs)  = BPLArray (map f xs)
mapObjRefs f (BPLSet     xs)  = BPLSet   (map f xs)
mapObjRefs f (BPLDict ks vs)  = BPLDict  (map f ks) (map f vs)
mapObjRefs f BPLNull          = BPLNull
mapObjRefs f BPLFill          = BPLFill
mapObjRefs f (BPLData     x)  = BPLData   x
mapObjRefs f (BPLDate     x)  = BPLDate   x
mapObjRefs f (BPLReal     x)  = BPLReal   x
mapObjRefs f (BPLInt      x)  = BPLInt    x
mapObjRefs f (BPLString   x)  = BPLString x
mapObjRefs f (BPLUID      x)  = BPLUID    x
mapObjRefs f (BPLBool     x)  = BPLBool   x

data UnparsedBPListRecord
    = UnparsedNull
    | UnparsedFill
    | MissingObjectRef Word64
    | UnparsedDict (Map Word64 Word64)
    | UnparsedSet [Word64]
    | UnparsedUID Integer
    deriving (Eq, Ord, Show)

data Abs
data Rel
data BPListRecords mode = BPListRecords
    { rootObject    :: Word64
    , records       :: Seq BPListRecord
    } deriving (Eq, Ord, Show)

data RawBPList = RawBPList
    { rawFile    :: ByteString
    , rawHeader  :: BPListHeader
    , rawOffsets :: Vector Word64
    , rawTrailer :: BPListTrailer
    } deriving (Eq, Ord, Show)

newtype BPListHeader = BPListHeader
    { bplistVersion     :: Word16
    } deriving (Eq, Ord, Show)
bplist00hdr = BPListHeader 0x3030

data BPListTrailer = BPListTrailer
    { sortVersion       :: !Word8
    , offsetIntSize     :: !Word8
    , objectRefSize     :: !Word8
    , numObjects        :: !Word64
    , topObject         :: !Word64
    , offsetTableOffset :: !Word64
    } deriving (Eq, Ord, Show)