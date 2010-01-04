{-# LANGUAGE
        FlexibleContexts
  #-}
module Data.PropertyList.Xml
    ( Plist
    , readXmlPlist, showXmlPlist
    
    , PlistItem
    , plistToPlistItem, plistItemToPlist
    
    , UnparsedPlistItem(..)
    , unparsedPlistItemToPlistItem

    , readXmlPropertyList, showXmlPropertyList
    , readXmlPlistFromFile, writeXmlPlistToFile
    , readXmlPropertyListFromFile, writeXmlPropertyListToFile
    
    ) where

import Data.PropertyList.Algebra
import Data.PropertyList.Xml.Parse
import Data.PropertyList.Xml.Types

-- * Reading and writing XML 'PartialPropertyList's and 'PropertyList's from 'String's

readXmlPropertyList :: (PListCoalgebra f PlistItem, TerminalPList f pl) => String -> Either String pl
readXmlPropertyList = fmap (toPlist . plistToPlistItem) . readXmlPlist

showXmlPropertyList :: (InitialPList f pl, PListAlgebra f PlistItem) => pl -> String
showXmlPropertyList = showXmlPlist . fromPlist


-- * Reading and writing XML 'Plist's from files

readXmlPlistFromFile :: FilePath -> IO (Either String Plist)
readXmlPlistFromFile path = do
        contents <- readFile path
        return (readXmlPlist contents)

writeXmlPlistToFile :: FilePath -> Plist -> IO ()
writeXmlPlistToFile path plist = do
        writeFile path (showXmlPlist plist)


-- * Reading and writing XML 'PartialPropertyList's and 'PropertyList's from files

-- |Read an XML propertylist from a file in the xml1 plist format to a
-- propertylist type which is terminal for the liftings supported by
-- 'PlistItem'  (such as @'PartialPropertyList' 'UnparsedPlistItem'@
-- or @'PartialPropertyList' 'PlistItem'@).
-- 
-- The 'TerminalPList' constraint is given to force a choice of lifting
-- for the property list coalgebra, since the lifting is not exposed in
-- this function's type.
readXmlPropertyListFromFile
  :: (PListCoalgebra f PlistItem, TerminalPList f pl) =>
     FilePath -> IO (Either String pl)
readXmlPropertyListFromFile file = do
        x <- readXmlPlistFromFile file
        return (fmap (toPlist . plistToPlistItem) x)

-- |Output a propertylist to a file in the xml1 plist format from an
-- initial propertylist type  (such as 'PropertyList', @'PartialPropertyList'
-- 'UnparsedPlistItem'@, or @'PartialPropertyList' 'PlistItem'@).
-- 
-- The 'InitialPList' constraint is given to force a choice of lifting
-- for the property list algebra, since the lifting is not exposed in
-- this function's type.
writeXmlPropertyListToFile
  :: (InitialPList f pl, PListAlgebra f PlistItem) =>
     FilePath -> pl -> IO ()
writeXmlPropertyListToFile file plist = do
        writeXmlPlistToFile file (fromPlist plist)
