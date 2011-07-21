{-# LANGUAGE EmptyDataDecls #-}
module Data.PropertyList.Binary.Types where

import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Sequence (Seq)
import Data.Time (UTCTime)
import Data.Vector.Unboxed (Vector)
import Data.Word

data BPListRecord
    = BPLNull
    | BPLFill
    | BPLArray [Word64]
    | BPLSet [Word64]
    | BPLData BS.ByteString
    | BPLDate UTCTime
    | BPLDict [Word64] [Word64]
    | BPLReal Double
    | BPLInt Integer
    | BPLString String
    | BPLUID Integer
    | BPLBool Bool
    deriving (Eq, Ord, Show)

mapObjRefs :: (Word64 -> Word64) -> BPListRecord -> BPListRecord
mapObjRefs f (BPLArray   xs)  = BPLArray (map f xs)
mapObjRefs f (BPLSet     xs)  = BPLSet   (map f xs)
mapObjRefs f (BPLDict ks vs)  = BPLDict  (map f ks) (map f vs)
mapObjRefs f other = other

data UnparsedBPListRecord
    = UnparsedNull
    | UnparsedFill
    | MissingObjectRef Word64
    | UnparsedDict [Word64] [Word64]
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
    { rawFile    :: BL.ByteString
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