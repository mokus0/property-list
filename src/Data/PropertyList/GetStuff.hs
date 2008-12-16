{-
 -      ``Data/PropertyList/GetStuff''
 -      (c) 2008 James Cook
 -}

module Data.PropertyList.GetStuff where

import Data.PropertyList.Type

import qualified Data.Map as M
import Data.Maybe

getDictionaryItem k (PLDict d) = M.lookup k d
getDictionaryItem _ _ = Nothing

getArrayItem i (PLArray a) = listToMaybe (drop i a)
getArrayItem _ _ = Nothing