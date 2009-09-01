{-# LANGUAGE 
    TemplateHaskell,
    FlexibleContexts, UndecidableInstances,
    TypeSynonymInstances, RelaxedPolyRec
  #-}

module Data.PropertyList.Type where

import Language.Haskell.TH.Fold

import qualified Data.Map as M
import Data.ByteString as B hiding (map)
import Data.Time

type PropertyList = PropertyList_ UnparsedPlistItem

data PropertyListS l m a
        = PLArray (l a)
        | PLData ByteString
        | PLDate UTCTime
        | PLDict  (m a)
        | PLReal Double
        | PLInt Integer
        | PLString String
        | PLBool Bool
        deriving (Eq, Ord, Show, Read)

foldPropertyListS :: (l a -> t)
                  -> (ByteString -> t)
                  -> (UTCTime -> t)
                  -> (m a -> t)
                  -> (Double -> t)
                  -> (Integer -> t)
                  -> (String -> t)
                  -> (Bool -> t) 
                  -> PropertyListS l m a -> t
foldPropertyListS = $(fold ''PropertyListS)

instance (Functor l, Functor m) => Functor (PropertyListS l m) where
    fmap f = foldPropertyListS (PLArray . fmap f) PLData PLDate (PLDict . fmap f) PLReal PLInt PLString PLBool

data M f a
    = S (f (M f a))
    | V a

foldM :: (f (M f a) -> t) -> (a -> t) -> M f a -> t
foldM = $(fold ''M)

instance (Eq (f (M f a)),  Eq a) => Eq (M f a) where
    S x == S y  = (x == y)
    V a == V b  = (a == b)
    _ == _ = False
instance (Ord (f (M f a)),  Ord a) => Ord (M f a) where
    S x `compare` S y  = x `compare` y
    V a `compare` V b  = a `compare` b
    S _ `compare` V _  = LT
    V _ `compare` S _  = GT
    
instance (Show (f (M f a)), Show a) => Show (M f a) where
    showsPrec p (S x) = showParen (p > 10) (showString "S " . showsPrec 11 x)
    showsPrec p (V x) = showParen (p > 10) (showString "V " . showsPrec 11 x)

-- instance Read...

type PropertyList_ = M (PropertyListS [] (M.Map String))

plArray     x   = S (PLArray  x)
plData      x   = S (PLData   x)
plDate      x   = S (PLDate   x)
plDict      x   = S (PLDict   x)
plReal      x   = S (PLReal   x)
plInt       x   = S (PLInt    x)
plString    x   = S (PLString x)
plBool      x   = S (PLBool   x)
plVar       x   = V x

data UnparsedPlistItem
        = UnparsedData String
        | UnparsedDate String
        | UnparsedInt  String
        | UnparsedReal String
        deriving (Eq, Ord, Show, Read)

instance Functor f => Functor (M f) where
    fmap f (S x) = S (fmap (fmap f) x)
    fmap f (V x) = V (f x)

instance Functor f => Monad (M f) where
    return = V
    (S x) >>= f = S (fmap (>>= f) x)
    (V x) >>= f = f x

-- TODO: if possible, make 'fold' in th-utils detect the Functor instances for
-- [] and Map k and automagically generate the call to fmap here
foldPropertyList :: (Functor list, Functor map)
                 => (list a -> a)
                 -> (ByteString -> a)
                 -> (UTCTime -> a)
                 -> (map a -> a)
                 -> (Double -> a)
                 -> (Integer -> a)
                 -> (String -> a)
                 -> (Bool -> a)
                 -> (t -> a)
                 -> M (PropertyListS list map) t -> a
foldPropertyList foldList a b foldMap c d e f g = foldIt
        where
                foldIt = foldM foldS g
                foldS = foldPropertyListS foldArray a b foldDict c d e f
                
                foldArray branches = foldList (fmap foldIt branches)
                foldDict dict = foldMap (fmap foldIt dict)
