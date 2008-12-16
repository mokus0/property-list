{-
 -      ``Data/PropertyList''
 -      rough cut type/reader/writer for Apple Property List format
 -}

module Data.PropertyList
    ( PropertyList
    , PropertyList_(..)
    , UnparsedPlistItem(..)
    , readPropertyListFromFile
    , writePropertyListToFile
    
    , module Data.PropertyList.GetStuff
    , module Data.PropertyList.SetStuff
    ) where

import Data.PropertyList.Type
import Data.PropertyList.Parse
import Data.PropertyList.Xml

import Data.PropertyList.GetStuff
import Data.PropertyList.SetStuff

readPropertyListFromFile :: FilePath -> IO (Either String PropertyList)
readPropertyListFromFile file = do
        x <- readPlistFromFile file
        return (fmap plistToPropertyList x)

writePropertyListToFile :: FilePath -> PropertyList -> IO ()
writePropertyListToFile file plist = do
        writePlistToFile file (propertyListToPlist unparsedPlistItemToPlistItem plist)
