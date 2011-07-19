{-# LANGUAGE
        FlexibleContexts
  #-}
module Data.PropertyList.Xml
    ( Plist
    , readXmlPlist, showXmlPlist
    , readXmlPlistFromFile, writeXmlPlistToFile
    
    , PlistItem
    , plistToPlistItem, plistItemToPlist
    
    , UnparsedPlistItem(..)
    , unparsedPlistItemToPlistItem
    
    , readXmlPropertyList, readXmlPropertyListFromFile
    
    , readXmlPartialPropertyList, showXmlPropertyList
    , readXmlPartialPropertyListFromFile, writeXmlPropertyListToFile
    
    ) where

import Data.Copointed
import Data.PropertyList.Algebra
import Data.PropertyList.Types
import Data.PropertyList.Xml.Parse
import Data.PropertyList.Xml.Types

import Control.Monad.Error ({- instance Monad (Either String) -})

-- * Reading and writing XML 'Plist's from files

-- |Try to parse a 'Plist' from an XML property-list file.
readXmlPlistFromFile :: FilePath -> IO (Either String Plist)
readXmlPlistFromFile path = do
        contents <- readFile path
        return (readXmlPlist contents)

-- |Try to write a 'Plist' to an XML property-list file.
writeXmlPlistToFile :: FilePath -> Plist -> IO ()
writeXmlPlistToFile path plist = do
        writeFile path (showXmlPlist plist)


-- * Reading and writing XML 'PartialPropertyList's and 'PropertyList's from 'String's

-- |Read an XML propertylist from a 'String' in the xml1 plist format to a
-- propertylist type which is terminal for the liftings supported by
-- 'PlistItem'  (such as @'PartialPropertyList' 'UnparsedPlistItem'@
-- or @'PartialPropertyList' 'PlistItem'@).
readXmlPartialPropertyList :: (PListCoalgebra f PlistItem, TerminalPList f pl) => String -> Either String pl
readXmlPartialPropertyList = fmap (toPlist . plistToPlistItem) . readXmlPlist

-- |Read a property list from a 'String' in the xml1 format.  If parsing
-- fails, returns a description of the problem in the 'Left' result.
readXmlPropertyList :: String -> Either String PropertyList
readXmlPropertyList str = do
    x <- readXmlPartialPropertyList str :: Either String (PartialPropertyList UnparsedPlistItem)
    completePropertyListByM (\unparsed -> Left ("Unparseable item found: " ++ show unparsed) :: Either String PropertyList) x

--readXmlPropertyList :: String -> PropertyList
--readXmlPropertyList
--    = runIdentity
--    . completePropertyListByM (\_ -> fail "parse error" :: Identity PropertyList)
--    . either error id
--    . (readXmlPartialPropertyList :: String -> Either String (PartialPropertyList UnparsedPlistItem))

-- |Render a propertylist to a 'String' in the xml1 plist format from any
-- initial propertylist type  (which includes 'PropertyList', @'PartialPropertyList'
-- 'UnparsedPlistItem'@, and @'PartialPropertyList' 'PlistItem'@).
showXmlPropertyList :: (InitialPList f pl, Functor f, Copointed f) => pl -> String
showXmlPropertyList
    = showXmlPlist
    . plistItemToPlist
    . fromPlist


-- * Reading and writing XML 'PartialPropertyList's and 'PropertyList's from files

-- |Read an XML propertylist from a file in the xml1 plist format to a
-- partial propertylist which is structurally sound but may contain some 
-- unparseable nodes.
readXmlPartialPropertyListFromFile
  :: FilePath -> IO (PartialPropertyList UnparsedPlistItem)
readXmlPartialPropertyListFromFile file = do
        x <- readXmlPlistFromFile file
        either fail (return . toPlist . plistToPlistItem) x

-- |Read a property list from a file in the xml1 format.  If parsing fails,
-- calls 'fail'.
readXmlPropertyListFromFile :: FilePath -> IO PropertyList
readXmlPropertyListFromFile file = do
    x <- readXmlPartialPropertyListFromFile file
    completePropertyListByM barf x
    where
        barf unparsed = fail ("Unparseable item found: " ++ show unparsed) :: IO PropertyList
    

-- |Output a propertylist to a file in the xml1 plist format from any
-- initial propertylist type  (which includes 'PropertyList', @'PartialPropertyList'
-- 'UnparsedPlistItem'@, and @'PartialPropertyList' 'PlistItem'@).
writeXmlPropertyListToFile
  :: (InitialPList f pl, Functor f, Copointed f) =>
     FilePath -> pl -> IO ()
writeXmlPropertyListToFile file plist = do
        writeXmlPlistToFile file (plistItemToPlist (fromPlist plist))
