module Xml where

import ArbitraryPList
import Data.PropertyList

prop_xml_roundtrip :: PropertyList -> Bool
prop_xml_roundtrip plist
    =  readXmlPropertyList (showXmlPropertyList plist)
    == Right plist