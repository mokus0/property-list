{-# LANGUAGE FlexibleContexts #-}
module Data.PropertyList.Xml
    ( UnparsedXmlPlistItem(..)
    , unparsedXmlPlistItemToElement
    
    , readXmlPropertyList, readXmlPropertyListFromFile
    
    , readXmlPartialPropertyList, showXmlPropertyList
    , readXmlPartialPropertyListFromFile, writeXmlPropertyListToFile
    
    ) where

import Control.Monad.Instances ({- instance Monad (Either e) -})
import Data.PropertyList.Algebra
import Data.PropertyList.Types
import Data.PropertyList.Xml.Algebra
import Text.XML.Light

-- * Reading and writing XML 'PartialPropertyList's and 'PropertyList's from 'String's

-- |Read an XML property list from a 'String' in the xml1 plist format, leaving 
-- unparseable elements in the tree.
readXmlPartialPropertyList :: String -> Either String (PartialPropertyList UnparsedXmlPlistItem)
readXmlPartialPropertyList str = case parseXMLDoc str of
    Just e@(Element (QName "plist" _ _) _ content _) ->
        if plistVersion e == "1.0"
            then case onlyElems content of
                [root]  -> Right (toPlist root)
                _       -> Left "plist element must have exactly one child element"
            else Left "plist version is not supported"
    Just e  -> Right (toPlist e)
    Nothing -> Left "not an XML document"
    where
        plistVersion = maybe "1.0" id . findAttrBy isVersion
        
        isVersion (QName "version" _ _) = True
        isVersion _ = False

-- |Read a property list from a 'String' in the xml1 format.  If parsing
-- fails, returns a description of the problem in the 'Left' result.
readXmlPropertyList :: String -> Either String PropertyList
readXmlPropertyList str
    =   readXmlPartialPropertyList str 
    >>= completePropertyListByM barf
        where barf unparsed = Left ("Unparseable item found: " ++ show unparsed) :: Either String PropertyList

-- |Render a propertylist to a 'String' in the xml1 plist format from any
-- initial propertylist type  (which includes 'PropertyList', @'PartialPropertyList'
-- 'UnparsedPlistItem'@, and @'PartialPropertyList' 'PlistItem'@).
showXmlPropertyList :: (InitialPList f pl, PListAlgebra f Element) => pl -> String
showXmlPropertyList = ppTopElement . fromPlist


-- * Reading and writing XML 'PartialPropertyList's and 'PropertyList's from files

-- |Read an XML propertylist from a file in the xml1 plist format to a
-- partial propertylist which is structurally sound but may contain some 
-- unparseable nodes.
readXmlPartialPropertyListFromFile
  :: FilePath -> IO (PartialPropertyList UnparsedXmlPlistItem)
readXmlPartialPropertyListFromFile file = do
    x <- readFile file
    either fail return (readXmlPartialPropertyList x)

-- |Read a property list from a file in the xml1 format.  If parsing fails,
-- calls 'fail'.
readXmlPropertyListFromFile :: FilePath -> IO PropertyList
readXmlPropertyListFromFile file = do
    x <- readFile file
    either fail return (readXmlPropertyList x)
    

-- |Output a propertylist to a file in the xml1 plist format from any
-- initial propertylist type  (which includes 'PropertyList', @'PartialPropertyList'
-- 'UnparsedPlistItem'@, and @'PartialPropertyList' 'PlistItem'@).
writeXmlPropertyListToFile :: FilePath -> PropertyList -> IO ()
writeXmlPropertyListToFile file plist =
    writeFile file (showXmlPropertyList plist)
