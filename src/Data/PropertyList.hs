{-# LANGUAGE FlexibleContexts #-}
module Data.PropertyList
    ( PropertyList
    , PartialPropertyList
    , PropertyListS(..)
    , UnparsedPlistItem(..)
    
    , PListAlgebra(..), PListCoalgebra(..)
    , InitialPList(..), TerminalPList(..)
    , fromPlist, toPlist
    , completePropertyList
    , completePropertyListBy, completePropertyListByM
    
    , readPropertyList
    , showPropertyList
    
    , readPropertyListFromFile
    , writePropertyListToFile
    
    , module Data.PropertyList.PropertyListItem
    ) where

import Data.PropertyList.Type
import Data.PropertyList.Parse
import Data.PropertyList.Xml
import Data.PropertyList.Object ({- instances -})

import Data.PropertyList.PropertyListItem

readPropertyList :: (PListCoalgebra f PlistItem, TerminalPList f pl) => String -> Either String pl
readPropertyList = fmap (toPlist . plistToPlistItem) . readPlist

showPropertyList :: (InitialPList f pl, PListAlgebra f PlistItem) => pl -> String
showPropertyList = showXml . fromPlist

-- |Read an XML propertylist from a file in the xml1 plist format to a
-- propertylist type which is terminal for the liftings supported by
-- 'PlistItem'  (such as @'PartialPropertyList' 'UnparsedPlistItem'@
-- or @'PartialPropertyList' 'PlistItem'@).
-- 
-- The 'TerminalPList' constraint is given to force a choice of lifting
-- for the property list coalgebra, since the lifting is not exposed in
-- this function's type.
readPropertyListFromFile
  :: (PListCoalgebra f PlistItem, TerminalPList f pl) =>
     FilePath -> IO (Either String pl)
readPropertyListFromFile file = do
        x <- readPlistFromFile file
        return (fmap (toPlist . plistToPlistItem) x)

-- |Output a propertylist to a file in the xml1 plist format from an
-- initial propertylist type  (such as 'PropertyList', @'PartialPropertyList'
-- 'UnparsedPlistItem'@, or @'PartialPropertyList' 'PlistItem'@).
-- 
-- The 'InitialPList' constraint is given to force a choice of lifting
-- for the property list algebra, since the lifting is not exposed in
-- this function's type.
writePropertyListToFile
  :: (InitialPList f pl, PListAlgebra f PlistItem) =>
     FilePath -> pl -> IO ()
writePropertyListToFile file plist = do
        writePlistToFile file (fromPlist plist)
