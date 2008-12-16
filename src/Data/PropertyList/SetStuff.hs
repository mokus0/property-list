{-
 -      ``Data/PropertyList/SetStuff''
 -      (c) 2008 James Cook
 -}

module Data.PropertyList.SetStuff where

import Data.PropertyList.Type

import qualified Data.Map as M
import Data.Maybe

setDictionaryItem k v (PLDict d) = (PLDict (M.insert k v d), True)
setDictionaryItem _ _ pl = (pl, False)
