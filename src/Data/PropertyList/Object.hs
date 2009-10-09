{-# LANGUAGE 
        FlexibleContexts, UndecidableInstances
  #-}
module Data.PropertyList.Object ({- instances only -}) where

import Data.Object
import Data.PropertyList.Type
import Data.PropertyList.Parse
import Data.PropertyList.Xml
import qualified Data.Map as M
import Control.Monad
import Control.Monad
import Control.Monad.Error

instance ToScalar UnparsedPlistItem where
    toScalar = toScalar . showXml . plistItemToPlist . unparsedPlistItemToPlistItem
instance ToObject UnparsedPlistItem where
    toObject = Scalar . toScalar

instance (ToObject (l a), ToObject (m a)) => ToObject (PropertyListS l m a) where
    toObject (PLArray a)    = toObject a
    toObject (PLData bs)    = toObject bs
    toObject (PLDate d)     = showToObject d
    toObject (PLDict d)     = toObject d
    toObject (PLReal d)     = showToObject d
    toObject (PLInt i)      = showToObject i
    toObject (PLString s)   = toObject s
    toObject (PLBool b)     = showToObject b

instance (ToObject (f (M f a)), ToObject a) => ToObject (M f a) where
    toObject (S x) = toObject x
    toObject (V a) = toObject a

instance (ToScalar k, ToObject v) => ToObject (M.Map k v) where
    toObject = toObject . M.assocs

showToObject :: Show a => a -> Object
showToObject = toObject . show

test :: Object
test = toObject (undefined :: PropertyList)
