module BPLists where

import ArbitraryPList
import CloseEnough
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.PropertyList

prop_binary_roundtrip :: PropertyList -> Bool
prop_binary_roundtrip plist
    =  readBinaryPropertyList (encodeBinaryPropertyList plist)
    ~= Right plist