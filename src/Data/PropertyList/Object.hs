{-# LANGUAGE 
        FlexibleContexts, UndecidableInstances, CPP
  #-}
module Data.PropertyList.Object ({- instances only -}) where

import Data.Object
import Data.PropertyList.Type
import Data.PropertyList.Parse
import Data.PropertyList.Xml
#ifdef data_object_uses_lazy_bytestrings
import Data.ByteString.Lazy.Char8
#else
import Data.ByteString.Char8
#endif
import qualified Data.Map as M
import Control.Monad
import Control.Monad
import Control.Monad.Error

instance ToScalar UnparsedPlistItem where
    toScalar = pack . showXml . plistItemToPlist . unparsedPlistItemToPlistItem
instance ToObject UnparsedPlistItem where
    toObject = Scalar . toScalar

instance (ToObject (l a), ToObject (m a)) => ToObject (PropertyListS l m a) where
    toObject (PLArray a)    = toObject a
    toObject (PLData bs)    = toObject bs
    toObject (PLDate d)     = showToScalar d
    toObject (PLDict d)     = toObject d
    toObject (PLReal d)     = showToScalar d
    toObject (PLInt i)      = showToScalar i
    toObject (PLString s)   = stringToScalar s
    toObject (PLBool b)     = showToScalar b

instance (ToObject (f (M f a)), ToObject a) => ToObject (M f a) where
    toObject (S x) = toObject x
    toObject (V a) = toObject a

instance (ToScalar k, ToObject v) => ToObject (M.Map k v) where
    toObject = toObject . M.assocs

stringToScalar = Scalar . pack
showToScalar :: Show a => a -> Object
showToScalar = stringToScalar . show

test = toObject (undefined :: PropertyList)