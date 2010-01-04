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
    
    , readXmlPropertyList
    , showXmlPropertyList
    
    , readXmlPropertyListFromFile
    , writeXmlPropertyListToFile
    
    , module Data.PropertyList.PropertyListItem
    ) where

import Data.PropertyList.Algebra
import Data.PropertyList.Types
import Data.PropertyList.Xml
import Data.PropertyList.Object ({- instances -})

import Data.PropertyList.PropertyListItem
