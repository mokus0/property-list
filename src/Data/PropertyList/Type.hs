{-
 -      ``Data/PropertyList/Type''
 -      (c) 2008 James Cook
 -}
{-# LANGUAGE 
    TemplateHaskell
  #-}

module Data.PropertyList.Type where

import Language.Haskell.TH.Fold

import qualified Data.Map as M
import Data.ByteString as B hiding (map)
import Data.Time

data PropertyList a
        = PLArray [PropertyList a]
        | PLData ByteString
        | PLDate UTCTime
        | PLDict  (M.Map String (PropertyList a))
        | PLReal Double
        | PLInt Integer
        | PLString String
        | PLBool Bool
        
        | PLVar a
        deriving (Eq, Ord, Show, Read)

instance Functor PropertyList where
        fmap f x = x >>= (return . f)

instance Monad PropertyList where
        return = PLVar
        x >>= f = foldPropertyList
                PLArray
                PLData
                PLDate
                PLDict
                PLReal
                PLInt
                PLString
                PLBool
                f x

-- TODO: if possible, make 'fold' in th-utils detect the Functor instances for
-- [] and Map k and automagically generate the call to fmap here
foldPropertyList :: ([a] -> a)
                 -> (ByteString -> a)
                 -> (UTCTime -> a)
                 -> (M.Map String a -> a)
                 -> (Double -> a)
                 -> (Integer -> a)
                 -> (String -> a)
                 -> (Bool -> a)
                 -> (t -> a)
                 -> PropertyList t -> a
foldPropertyList foldList a b foldMap c d e f g = foldIt
        where
                foldIt = $(fold ''PropertyList) foldArray a b foldDict c d e f g
                
                foldArray branches = foldList (fmap foldIt branches)
                foldDict dict = foldMap (fmap foldIt dict)
