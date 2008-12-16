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

type PropertyList = PropertyList_ UnparsedPlistItem

data PropertyList_ a
        = PLArray [PropertyList_ a]
        | PLData ByteString
        | PLDate UTCTime
        | PLDict  (M.Map String (PropertyList_ a))
        | PLReal Double
        | PLInt Integer
        | PLString String
        | PLBool Bool
        
        | PLVar a
        deriving (Eq, Ord, Show, Read)

data UnparsedPlistItem
        = UnparsedData String
        | UnparsedDate String
        | UnparsedInt  String
        | UnparsedReal String
        deriving (Eq, Ord, Show, Read)

instance Functor PropertyList_ where
        fmap f x = x >>= (return . f)

instance Monad PropertyList_ where
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
                 -> PropertyList_ t -> a
foldPropertyList foldList a b foldMap c d e f g = foldIt
        where
                foldIt = $(fold ''PropertyList_) foldArray a b foldDict c d e f g
                
                foldArray branches = foldList (fmap foldIt branches)
                foldDict dict = foldMap (fmap foldIt dict)
