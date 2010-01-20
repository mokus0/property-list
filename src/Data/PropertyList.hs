module Data.PropertyList
    ( -- * The basic property list types
      -- |Property lists have several supported representations, but the main
      -- one most end users will care about is 'PropertyList'.  This is a basic
      -- algebraic representation of property lists which can be constructed
      -- using the polymorphic constructors described below and pattern-matched
      -- using the polymorphic destructors (designed for convenient usage with
      -- the ViewPatterns extension)
      --
      -- Also provided is the 'PartialPropertyList' representation, which is 
      -- a property list that is extended by adding a new constructor containing
      -- some arbitrary type chosen by the user.  It is mostly used as an
      -- intermediate represenation for property lists that have been parsed
      -- into their overall shape but not all elements have been parsed into
      -- their final format.
      -- 
      -- The 'UnparsedPlistItem' type is the type most often used with
      -- 'PartialPropertyList', and that is really its only purpose - to
      -- represent unparseable items from an XML plist during intermediate
      -- stages of translation.
      PropertyList
    , PartialPropertyList
    , PropertyListS(..)
    , UnparsedPlistItem(..)
      
    -- * Constructors and destructors for property lists
    -- |The \"pl*\" operations construct 'PropertyList's, 'PartialPropertyList's, 
    -- or any other types defining an unlifted algebra for the 'PropertyListS'
    -- signature.
    --
    -- The \"fromPl*\" operations are ViewPattern matching operations for
    -- 'PropertyList', 'PartialPropertyList', or any other type defining
    -- a 'Maybe'-lifted coalgebra for 'PropertyListS'.
    --
    -- The generality of these operations means that they can also be used
    -- to directly generate or analyze \"external\" formats such as the XML
    -- 'Plist' representation.
    , plArray,   fromPlArray
    , plData,    fromPlData
    , plDate,    fromPlDate
    , plDict,    fromPlDict
    , plReal,    fromPlReal
    , plInt,     fromPlInt
    , plString,  fromPlString
    , plBool,    fromPlBool
    
    -- * The internal algebraic model for property lists
    -- | Internally, conversions between various property list representations 
    -- are all defined in terms of universal algebra, which is basically just
    -- fancy math-talk for \"very general interfaces that let you convert between 
    -- certain kinds of representations easily and efficiently\".  
    -- 
    -- Most users do not need to understand this stuff - the class names are
    -- only exported because they appear in the types of the constructors and
    -- destructors.  For more detailed info, see "Data.PropertyList.Algebra".
    , PListAlgebra, PListCoalgebra
    , InitialPList, TerminalPList

    -- * Parsing and formatting property lists using any supported format
    , readPropertyList
    , showPropertyList
    
    , readPropertyListFromFile
    , writePropertyListToFile
    
    -- * Parsing and formatting property lists using the XML format
    , readXmlPropertyList
    , showXmlPropertyList
    
    , readXmlPropertyListFromFile
    , writeXmlPropertyListToFile
    
    -- * Manipulating property lists
    -- 
    -- |This module exports a class ('PropertyListItem') and several functions
    -- used to manipulate 'PropertyList's and their contents at a high level,
    -- viewing and manipulating the data in the tree through ad-hoc 
    -- transformations.
    -- 
    -- For example, consider the following property list:
    --
    -- > myPlist = plDict $ M.fromList
    -- >    [ ("foo", plInt 4)
    -- >    , ("bar", plString "qux")
    -- >    , ("subDict", plDict $ M.fromList
    -- >         [ ("item 1", plString "This is item 1!")
    -- >         , ("item B", plBool True)
    -- >         ])
    -- >    ] :: PropertyList
    -- 
    -- Some typical actions you might do with a plist like this (Note that
    -- in many cases a 'Just' is added - this is because the key-path operations
    -- are defined in terms of 'Maybe' so that operations like 'setItemAtKeyPath'
    -- or 'alterItemAtKeyPath' can create new items where none already exist):
    -- 
    -- > getItemAtKeyPath ["subDict", "item B"] (Just myPlist) :: Maybe Bool
    -- 
    --      (returns @Just True@)
    -- 
    -- > getItemAtKeyPath ["subDict"] (Just myPlist) :: M.Map String String
    -- 
    --      (returns @M.fromList [(\"item 1\", \"This is item 1!\"), (\"item B\", \"YES\")]@.  
    --       Note the stringification of non-string items.  In general, 'PropertyListItem'
    --       instances are expected to do \"reasonable\" conversions to try and make sense
    --       of what the user is asking the system to do.)
    -- 
    -- > setItemAtKeyPath ["omg", "lolwut"] (Just "roflcopter") (Just myPlist)
    -- 
    --      (returns a modified version of @myPlist@ with 
    --      @plDict $ M.fromList [(\"omg\", plDict $ M.fromList [(\"omg\", 
    --       plDict $ M.fromList [(\"lolwut\", plString \"roflcopter\")])])]@
    --       added to the root dictionary)
    --
    -- > setItemAtKeyPath ["foo"] Nothing (Just myPlist)
    -- 
    --      (returns a modified version of @myPlist@ with the \"foo\" entry in the
    --       root dictionary deleted)
    -- 
    -- > setItemAtKeyPath ["foo", "bar", "baz"] (Just "qux") Nothing
    -- 
    --      (returns a new dictionary with @plString \"qux\"@ at the key path foo.bar.baz)
    --

    , module Data.PropertyList.PropertyListItem
    , module Data.PropertyList.KeyPath
    ) where

import Data.PropertyList.Algebra
import Data.PropertyList.Types
import Data.PropertyList.Xml

import Data.PropertyList.PropertyListItem
import Data.PropertyList.KeyPath

-- | Read a property list from a 'String', trying all supported property list formats.
-- Presently, only the \"XML1\" format is supported.  See also 'readXmlPropertyList'.
readPropertyList :: String -> Either String PropertyList
readPropertyList = readXmlPropertyList

-- | Write a property list to a 'String', using a \"preferred\" property list format.
-- Presently, that is the \"XML1\" format.  See also 'showXmlPropertyList'.
showPropertyList :: PropertyList -> String
showPropertyList = showXmlPropertyList

-- | Read a property list from a file, trying all supported property list formats.
-- Presently, only the \"XML1\" format is supported.  See also
-- 'readXmlPropertyListFromFile'.
readPropertyListFromFile :: FilePath -> IO PropertyList
readPropertyListFromFile = readXmlPropertyListFromFile

-- | Write a property list to a file, using a \"preferred\" property list format.
-- Presently, that is the \"XML1\" format.  See also 'writeXmlPropertyListToFile'.
writePropertyListToFile :: FilePath -> PropertyList -> IO ()
writePropertyListToFile = writeXmlPropertyListToFile
