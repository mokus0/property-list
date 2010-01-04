{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
        ViewPatterns
  #-}

-- |This module provides plist-algebras and -coalgebras for primitive types
-- such as strings, numbers, lists, arrays, etc.
--
-- These instances can be used in conjunction with the 
-- "Data.PropertyList.PropertyListItem" interface to perform structured
-- editing deep inside a property list.
--
-- Not entirely sure these instances are a good design choice.  There will
-- need to be a 'PropertyListItem' concept anyway, since the list and dict
-- cases can't be encoded like this anyway.
module Data.PropertyList.Algebra.Primitives where

import Data.PropertyList.Algebra
import Control.Functor.Pointed

import qualified Data.Map as M
import Data.Char
import Data.ByteString.Class
import Control.Monad.Identity

-- * Lists, arrays, dictionaries, etc.
    -- oops... can't work!  It's not a primitive-recursive/-corecursive
    -- operation, since a list is represented by at least 2 levels in the
    -- PList, possibly infinitely many.

--  instance PListCoalgebra Identity t => PListCoalgebra PropertyListS [t] where
--      plistCoalgebra = PLArray . ?x . map (runIdentity . plistCoalgebra)

-- * ByteStrings

-- * Dates/Times

-- * Fractional numbers
instance Copointed f => PListAlgebra f (Maybe Double) where
    plistAlgebra pl = case extract pl of
        PLReal d                                -> Just d
        PLInt i                                 -> Just (fromInteger i)
        PLString (reads        -> [(d, "")])    -> Just d
        _                                       -> Nothing

instance Pointed f => PListCoalgebra f Double where
    plistCoalgebra = point . PLReal

instance Copointed f => PListAlgebra f (Maybe Float) where
    plistAlgebra = fmap fromDouble . plistAlgebra . fmap (fmap (fmap toDouble))
        where
            fromDouble = realToFrac :: Double -> Float
            toDouble   = realToFrac :: Float -> Double

instance Pointed f => PListCoalgebra f Float where
    plistCoalgebra = fmap (fmap fromDouble) . plistCoalgebra . toDouble
        where
            fromDouble = realToFrac :: Double -> Float
            toDouble   = realToFrac :: Float -> Double

instance Copointed f => PListAlgebra f (Maybe Rational) where
    plistAlgebra = fmap fromDouble . plistAlgebra . fmap (fmap (fmap toDouble))
        where
            fromDouble = realToFrac :: Double -> Rational
            toDouble   = realToFrac :: Rational -> Double

instance Pointed f => PListCoalgebra f Rational where
    plistCoalgebra = fmap (fmap fromDouble) . plistCoalgebra . toDouble
        where
            fromDouble = realToFrac :: Double -> Rational
            toDouble   = realToFrac :: Rational -> Double

-- * Integral numbers
instance Copointed f => PListAlgebra f (Maybe Integer) where
    plistAlgebra pl = case extract pl of
        PLInt i                                 -> Just i
        PLReal (properFraction ->  (i,  0))     -> Just i
        PLString (reads        -> [(i, "")])    -> Just i
        _                                       -> Nothing

instance Pointed f => PListCoalgebra f Integer where
    plistCoalgebra = point . PLInt

instance Copointed f => PListAlgebra f (Maybe Int) where
    plistAlgebra = fmap fromInteger . plistAlgebra . fmap (fmap (fmap toInteger))

instance Pointed f => PListCoalgebra f Int where
    plistCoalgebra = fmap (fmap fromInteger) . plistCoalgebra . toInteger

-- * 'String's
instance Copointed f => PListAlgebra f (Maybe String) where
    plistAlgebra pl = case extract pl of
        PLBool b -> Just (if b then "YES" else "NO")
        PLData x -> Just (fromStrictByteString x)
        PLInt i  -> Just (show i)
        PLReal d -> Just (show d)
        PLString s -> Just s
        _ -> Nothing

instance Pointed f => PListCoalgebra f String where
    plistCoalgebra = point . PLString

-- * 'Bool's
instance Copointed f => PListAlgebra f (Maybe Bool) where
    plistAlgebra pl = case extract pl of
        PLBool b -> Just b
        PLString b
            | map toLower b `elem` ["yes", "true"]
            -> Just True
            | map toLower b `elem` ["no", "false"]
            -> Just False
        _ -> Nothing

instance Pointed f => PListCoalgebra f Bool where
    plistCoalgebra = point . PLBool
