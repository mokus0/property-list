{-
 -      ``Data/PropertyList/Parse''
 -      (c) 2008 James Cook
 -}
{-# LANGUAGE 
    TemplateHaskell
  #-}

module Data.PropertyList.Parse where

import Language.Haskell.TH.Fold

import Prelude as P
import Data.PropertyList.Xml.Dtd as X
import Data.PropertyList.Xml
import Data.PropertyList.Type

import qualified Data.Map as M
import Data.ByteString as B hiding (map)
import Data.Time
import System.Locale
import Codec.Binary.Base64 as B64

import Text.XML.HaXml.OneOfN
import Text.XML.HaXml.XmlContent

-- run an incremental parser - a function which takes
-- a token type and returns either a subterm (possibly still
-- containing unparsed tokens) or an unparsed token (possibly
-- a new token, maybe even of a new type).  This interpretation
-- of the action of this function is based on a term-algebra
-- view of the monad in question.
parseT :: Monad t => (a -> Either (t a) b) -> a -> t b
parseT f = parse
        where parse token =
                either (>>= parse) return (f token)

data UnparsedPlistItem
        = UnparsedData String
        | UnparsedDate String
        | UnparsedInt  String
        | UnparsedReal String
        deriving (Eq, Ord, Show, Read)

unparsedPlistItemToPlistItem :: UnparsedPlistItem -> PlistItem
unparsedPlistItemToPlistItem = $(fold ''UnparsedPlistItem)
        (TwoOf9   . Data    )
        (ThreeOf9 . Date    )
        (SixOf9   . AInteger)
        (FiveOf9  . AReal   )

plistToPropertyList :: Plist -> PropertyList UnparsedPlistItem
plistToPropertyList = parseT parsePlistItem . plistToPlistItem

plistItemToPropertyList :: PlistItem -> PropertyList UnparsedPlistItem
plistItemToPropertyList = plistToPropertyList . plistItemToPlist

parsePlistItem :: PlistItem -> Either (PropertyList PlistItem) UnparsedPlistItem
parsePlistItem item = case item of
        OneOf9   (Array x   )   -> accept PLArray (map return x)
        TwoOf9   (Data x    )   -> case decode x of 
                Just d                  -> accept PLData (pack d)
                Nothing                 -> reject UnparsedData x
        ThreeOf9 (Date x    )   -> case parseTime defaultTimeLocale dateFormat x of
                Just t                  -> accept PLDate t
                Nothing                 -> reject UnparsedDate x
        FourOf9  (Dict x    )   -> accept PLDict (M.fromList [ (k, return v) | Dict_ (Key k) v <- x])
        FiveOf9  (AReal x   )   -> tryRead PLReal UnparsedReal x
        SixOf9   (AInteger x)   -> tryRead PLInt  UnparsedInt x
        SevenOf9 (AString  x)   -> accept PLString x
        EightOf9 (X.True    )   -> accept PLBool P.True
        NineOf9  (X.False   )   -> accept PLBool P.False
        
        where
                accept :: (a -> b) -> a -> Either b c
                accept con = Left  . con
                
                reject :: (a -> c) -> a -> Either b c
                reject con = Right . con
                
                tryRead :: Read a => (a -> b) -> (String -> c) -> String -> Either b c
                tryRead onGood onBad str =
                        case reads str of
                                ((result, ""):_) -> accept onGood result
                                _                -> reject onBad  str
                       
               
dateFormat :: String
dateFormat = "%FT%TZ"

propertyListToPlist :: (a -> PlistItem) -> PropertyList a -> Plist
propertyListToPlist fromOther = plistItemToPlist . propertyListToPlistItem fromOther

propertyListToPlistItem :: (a -> PlistItem) -> PropertyList a -> PlistItem
propertyListToPlistItem fromOther = foldPropertyList
          (\x -> OneOf9 (Array x)
        ) (\x -> TwoOf9 (Data (encode (unpack x)))
        ) (\x -> ThreeOf9 (Date (formatTime defaultTimeLocale dateFormat x))
        ) (\x -> FourOf9 (Dict [Dict_ (Key k) v | (k,v) <- M.toList x])
        ) (\x -> FiveOf9 (AReal (show x))
        ) (\x -> SixOf9 (AInteger (show x))
        ) (\x -> SevenOf9 (AString x)
        ) (\x -> if x then EightOf9 X.True else NineOf9 X.False
        ) fromOther