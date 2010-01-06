{-# LANGUAGE 
    TemplateHaskell, CPP,
    MultiParamTypeClasses,
    FlexibleContexts, FlexibleInstances, TypeSynonymInstances,
    UndecidableInstances, OverlappingInstances, IncoherentInstances
  #-}

module Data.PropertyList.Xml.Parse where

import Language.Haskell.TH.Fold

import Prelude as P
#ifdef HaXml_1_13
import Data.PropertyList.Xml.Dtd_1_13 as X
#else
import Data.PropertyList.Xml.Dtd as X
#endif

import Data.PropertyList.Algebra
import Data.PropertyList.Xml.Types

import Control.Functor.Pointed
import Control.Arrow ((+++))
import Control.Monad.Identity
import qualified Data.Map as M
import Data.ByteString as B hiding (map)
import Data.Time
import System.Locale
import Codec.Binary.Base64 as B64

import Text.XML.HaXml.OneOfN

-- |A representation of values that were structurally sound in the 
-- property list file but the contents of which couldn't be interpreted
-- as what they claimed to be.  The result of the initial parse phase will
-- typically be a @PartialPropertyList UnparsedPlistItem@, and if
-- the whole plist was parsed properly will contain no actual values 
-- of this type.
data UnparsedPlistItem
    = UnparsedData String
    | UnparsedDate String
    | UnparsedInt  String
    | UnparsedReal String
    deriving (Eq, Ord, Show, Read)

dateFormat :: String
dateFormat = "%FT%TZ"

-- This instance is not efficient, and should really only be used as a convenience
-- to allow direct construction of 'Plist's using the \"smart constructors\"
instance PListAlgebra f PlistItem => PListAlgebra f Plist where
    plistAlgebra = plistItemToPlist . plistAlgebra . fmap (fmap plistToPlistItem)

instance Copointed f => PListAlgebra f PlistItem where
    {-# SPECIALIZE instance PListAlgebra Identity PlistItem #-}
    plistAlgebra = foldPropertyListS
          (\x -> OneOf9 (Array x)
        ) (\x -> TwoOf9 (Data (encode (unpack x)))
        ) (\x -> ThreeOf9 (Date (formatTime defaultTimeLocale dateFormat x))
        ) (\x -> FourOf9 (Dict [Dict_ (Key k) v | (k,v) <- M.toList x])
        ) (\x -> FiveOf9 (AReal (show x))
        ) (\x -> SixOf9 (AInteger (show x))
        ) (\x -> SevenOf9 (AString x)
        ) (\x -> if x then EightOf9 X.True else NineOf9 X.False
        ) . extract

instance PListAlgebra   (Either UnparsedPlistItem) PlistItem where
    plistAlgebra (Left unparsed) = unparsedPlistItemToPlistItem unparsed
    plistAlgebra (Right parsed) = plistAlgebra (Identity parsed)

instance PListAlgebra   (Either PlistItem) PlistItem where
    plistAlgebra (Left unparsed) = unparsed
    plistAlgebra (Right parsed) = plistAlgebra (Identity parsed)

instance PListCoalgebra (Either UnparsedPlistItem) PlistItem where
    plistCoalgebra item = case item of
        OneOf9   (Array x   )   -> accept PLArray x
        TwoOf9   (Data x    )   -> case decode x of 
                Just d                  -> accept PLData (pack d)
                Nothing                 -> reject UnparsedData x
        ThreeOf9 (Date x    )   -> case parseTime defaultTimeLocale dateFormat x of
                Just t                  -> accept PLDate t
                Nothing                 -> reject UnparsedDate x
        FourOf9  (Dict x    )   -> accept PLDict (M.fromList [ (k, v) | Dict_ (Key k) v <- x])
        FiveOf9  (AReal x   )   -> tryRead PLReal UnparsedReal x
        SixOf9   (AInteger x)   -> tryRead PLInt  UnparsedInt x
        SevenOf9 (AString  x)   -> accept PLString x
        EightOf9 (X.True    )   -> accept PLBool P.True
        NineOf9  (X.False   )   -> accept PLBool P.False
        
        where
                accept :: (a -> c) -> a -> Either b c
                accept con = Right . con
                
                reject :: (a -> b) -> a -> Either b c
                reject con = Left  . con
                
                tryRead :: Read a => (a -> c) -> (String -> b) -> String -> Either b c
                tryRead onGood onBad str =
                        case reads str of
                                ((result, ""):_) -> accept onGood result
                                _                -> reject onBad  str

instance PListCoalgebra (Either PlistItem) PlistItem where
    plistCoalgebra = (unparsedPlistItemToPlistItem +++ id) . plistCoalgebra

instance PListCoalgebra Maybe PlistItem where
    plistCoalgebra = either (const Nothing) Just . (plistCoalgebra :: PlistItem -> Either PlistItem (PropertyListS PlistItem))

-- This instance is not efficient, and should really only be used as a convenience
-- to allow direct deconstruction of 'Plist's using the \"view deconstructors\"
instance PListCoalgebra f PlistItem => PListCoalgebra f Plist where
    plistCoalgebra = fmap (fmap plistItemToPlist) . plistCoalgebra . plistToPlistItem

-- |Take the unparsed data from an 'UnparsedPlistItem' and wrap it in
-- the appropriate 'PlistItem' constructor.
unparsedPlistItemToPlistItem :: UnparsedPlistItem -> PlistItem
unparsedPlistItemToPlistItem = $(fold ''UnparsedPlistItem)
        (TwoOf9   . Data    )
        (ThreeOf9 . Date    )
        (SixOf9   . AInteger)
        (FiveOf9  . AReal   )
