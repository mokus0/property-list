module BPLists where

import ArbitraryPList
import CloseEnough
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.PropertyList

strictifyBS = BS.concat . BL.toChunks

prop_binary_roundtrip :: PropertyList -> Bool
prop_binary_roundtrip plist
    =  readBinaryPropertyList (strictifyBS (encodeBinaryPropertyList plist))
    ~= Right plist