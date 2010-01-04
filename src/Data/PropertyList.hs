module Data.PropertyList
    ( PropertyList
    , PropertyList_
    , UnparsedPlistItem(..)
    , readPropertyListFromFile
    , writePropertyListToFile
    
    , module Data.PropertyList.PropertyListItem
    ) where

import Data.PropertyList.Type
import Data.PropertyList.Parse
import Data.PropertyList.Xml
import Data.PropertyList.Object ({- instances -})

import Data.PropertyList.PropertyListItem

readPropertyListFromFile :: FilePath -> IO (Either String PropertyList)
readPropertyListFromFile file = do
        x <- readPlistFromFile file
        return (fmap plistToPropertyList x)

writePropertyListToFile :: FilePath -> PropertyList -> IO ()
writePropertyListToFile file plist = do
        writePlistToFile file (propertyListToPlist unparsedPlistItemToPlistItem plist)
