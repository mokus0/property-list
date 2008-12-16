{-
 -      ``Data/PropertyList/PropertyListItem''
 -      (c) 2008 James Cook
 -}
{-# LANGUAGE
    TypeSynonymInstances
  #-}

module Data.PropertyList.PropertyListItem where

import Data.PropertyList.Type

class PropertyListItem i where
    toPropertyList :: i -> PropertyList
    fromPropertyList :: PropertyList -> Maybe i

alterPropertyList :: PropertyListItem i => (Maybe i -> Maybe i) 
    -> PropertyList -> Maybe PropertyList
alterPropertyList f = fmap toPropertyList . f . fromPropertyList

instance PropertyListItem String where
    toPropertyList = PLString
    fromPropertyList (PLString x) = Just x
    fromPropertyList _ = Nothing

instance PropertyListItem Integer where
    toPropertyList = PLInt
    fromPropertyList (PLInt i) = i

instance PropertyListItem Double where
    toPropertyList = PLReal
    fromPropertyList (PLReal d) = d

