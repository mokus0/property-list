{-# LANGUAGE EmptyDataDecls #-}
module Data.PropertyList.Binary.Types where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Time (UTCTime)
import Data.Vector.Unboxed (Vector)
import Data.Word

data BPListRecord i
    = BPLNull
    | BPLFill
    | BPLArray [i]
    | BPLSet [i]
    | BPLData ByteString
    | BPLDate UTCTime
    | BPLDict [i] [i]
    | BPLReal Double
    | BPLInt Integer
    | BPLString String
    | BPLUID Integer
    | BPLBool Bool
    deriving (Eq, Ord, Show)

instance Functor BPListRecord where
    fmap f (BPLArray   xs)  = BPLArray (map f xs)
    fmap f (BPLSet     xs)  = BPLSet   (map f xs)
    fmap f (BPLDict ks vs)  = BPLDict  (map f ks) (map f vs)
    fmap f BPLNull          = BPLNull
    fmap f BPLFill          = BPLFill
    fmap f (BPLData     x)  = BPLData   x
    fmap f (BPLDate     x)  = BPLDate   x
    fmap f (BPLReal     x)  = BPLReal   x
    fmap f (BPLInt      x)  = BPLInt    x
    fmap f (BPLString   x)  = BPLString x
    fmap f (BPLUID      x)  = BPLUID    x
    fmap f (BPLBool     x)  = BPLBool   x

data UnparsedBPListRecord i
    = UnparsedNull
    | UnparsedFill
    | MissingObjectRef i
    | UnparsedDict (Map i i)
    | UnparsedSet [i]
    | UnparsedUID Integer
    deriving (Eq, Ord, Show)

data Abs
data Rel
data BPListRecords mode i = BPListRecords
    { rootObject    :: i
    , records       :: Seq (BPListRecord i)
    } deriving (Eq, Ord, Show)

instance Functor (BPListRecords mode) where
    fmap f (BPListRecords root records)
        = BPListRecords (f root) (fmap (fmap f) records)

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