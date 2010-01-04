{-# LANGUAGE 
        MultiParamTypeClasses,
        FlexibleInstances, FlexibleContexts, UndecidableInstances,
        TypeSynonymInstances,
        OverlappingInstances, IncoherentInstances
  #-}
module Data.PropertyList.Object ({- instances only -}) where

import Data.Object
import Data.PropertyList.Type
import Data.PropertyList.Parse
import Data.PropertyList.Xml
import qualified Data.Map as M
import Control.Functor.Pointed
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error

instance ToScalar UnparsedPlistItem where
    toScalar = toScalar . showXml . plistItemToPlist . unparsedPlistItemToPlistItem
instance ToObject UnparsedPlistItem where
    toObject = Scalar . toScalar

-- instance (ToObject a, ToObject [a]) => ToObject (PropertyListS a) where
--     toObject = fromPlist

instance ToObject PropertyList where
    toObject = fromPlist

instance (ToScalar k, ToObject v) => ToObject (M.Map k v) where
    toObject = toObject . M.assocs

showToObject :: Show a => a -> Object
showToObject = toObject . show

instance Copointed f => PListAlgebra f Object where
    plistAlgebra x = case extract x of
        PLArray a   -> toObject a
        PLData bs   -> toObject bs
        PLDate d    -> showToObject d
        PLDict d    -> toObject d
        PLReal d    -> showToObject d
        PLInt i     -> showToObject i
        PLString s  -> toObject s
        PLBool b    -> showToObject b

instance ToObject a => PListAlgebra (Either a) Object where
    plistAlgebra (Left  x) = toObject x
    plistAlgebra (Right x) = plistAlgebra (Identity x)
