module Data.PropertyList.Binary
    ( BPListHeader(..), BPListTrailer(..)
    , BPListRecord(..), BPListRecords(..)
    , readBPListRecords, putBPList
    
    , Abs, Rel
    , linearize, delinearize, absolutize, intern
    
    , UnparsedBPListRecord(..)
    , readBinaryPartialPropertyList
    , readBinaryPartialPropertyListFromFile
    
    , readBinaryPropertyList
    , readBinaryPropertyListFromFile
    
    , encodeBinaryPropertyList
    , writeBinaryPropertyListToFile
    ) where

import Control.Applicative
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.PropertyList.Types
import Data.PropertyList.Binary.Algebra ({- instances -})
import Data.PropertyList.Binary.Linearize
import Data.PropertyList.Binary.Parse
import Data.PropertyList.Binary.Put
import Data.PropertyList.Binary.Types
import Data.Word

readBinaryPartialPropertyList :: BS.ByteString -> Either String (PartialPropertyList (UnparsedBPListRecord Word64))
readBinaryPartialPropertyList bs = do
    delinearize <$> readBPListRecords bs

readBinaryPropertyList :: BS.ByteString -> Either String PropertyList
readBinaryPropertyList bs = do
    readBinaryPartialPropertyList bs >>= completePropertyListByM barf
    where barf unparsed = Left ("Unparseable item found: " ++ show unparsed) :: Either String PropertyList

readBinaryPropertyListFromFile :: FilePath -> IO PropertyList
readBinaryPropertyListFromFile path = do
    contents <- BS.readFile path
    either fail return (readBinaryPropertyList contents)

readBinaryPartialPropertyListFromFile :: FilePath -> IO (PartialPropertyList (UnparsedBPListRecord Word64))
readBinaryPartialPropertyListFromFile file = do
    bs <- BS.readFile file
    either fail return (readBinaryPartialPropertyList bs)

encodeBinaryPropertyList :: PropertyList -> BL.ByteString
encodeBinaryPropertyList = runPut . putBPList . linearize

writeBinaryPropertyListToFile :: FilePath -> PropertyList -> IO ()
writeBinaryPropertyListToFile path = 
    BL.writeFile path . encodeBinaryPropertyList