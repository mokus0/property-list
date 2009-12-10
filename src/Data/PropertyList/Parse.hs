{-# LANGUAGE 
    TemplateHaskell, CPP
  #-}

module Data.PropertyList.Parse where

import Language.Haskell.TH.Fold

import Prelude as P
#ifdef HaXml_1_13
import Data.PropertyList.Xml.Dtd_1_13 as X
#else
import Data.PropertyList.Xml.Dtd as X
import Text.XML.HaXml.XmlContent
#endif

import Data.PropertyList.Xml
import Data.PropertyList.Type

import qualified Data.Map as M
import Data.ByteString as B hiding (map)
import Data.Time
import System.Locale
import Codec.Binary.Base64 as B64

import Text.XML.HaXml.OneOfN

-- |run an incremental parser - a function which takes
-- a token type and returns either a subterm (possibly still
-- containing unparsed tokens) or an unparseable token (possibly
-- a new token, maybe even of a new type).  This interpretation
-- of the action of this function is based on a term-algebra
-- view of the monad in question, where the 'variable' sort
-- consists of unparsed fragments of the source data.
parseT :: Monad t => (a -> Either (t a) b) -> a -> t b
parseT f = parse
        where parse token =
                either (>>= parse) return (f token)

unparsedPlistItemToPlistItem :: UnparsedPlistItem -> PlistItem
unparsedPlistItemToPlistItem = $(fold ''UnparsedPlistItem)
        (TwoOf9   . Data    )
        (ThreeOf9 . Date    )
        (SixOf9   . AInteger)
        (FiveOf9  . AReal   )

plistToPropertyList :: Plist -> PropertyList
plistToPropertyList = parseT parsePlistItem . plistToPlistItem

plistItemToPropertyList :: PlistItem -> PropertyList
plistItemToPropertyList = plistToPropertyList . plistItemToPlist

parsePlistItem :: PlistItem -> Either (PropertyList_ PlistItem) UnparsedPlistItem
parsePlistItem item = case item of
        OneOf9   (Array x   )   -> accept plArray (map return x)
        TwoOf9   (Data x    )   -> case decode x of 
                Just d                  -> accept plData (pack d)
                Nothing                 -> reject UnparsedData x
        ThreeOf9 (Date x    )   -> case parseTime defaultTimeLocale dateFormat x of
                Just t                  -> accept plDate t
                Nothing                 -> reject UnparsedDate x
        FourOf9  (Dict x    )   -> accept plDict (M.fromList [ (k, return v) | Dict_ (Key k) v <- x])
        FiveOf9  (AReal x   )   -> tryRead plReal UnparsedReal x
        SixOf9   (AInteger x)   -> tryRead plInt  UnparsedInt x
        SevenOf9 (AString  x)   -> accept plString x
        EightOf9 (X.True    )   -> accept plBool P.True
        NineOf9  (X.False   )   -> accept plBool P.False
        
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

propertyListToPlist :: (a -> PlistItem) -> PropertyList_ a -> Plist
propertyListToPlist fromOther = plistItemToPlist . propertyListToPlistItem fromOther

propertyListToPlistItem :: (a -> PlistItem) -> PropertyList_ a -> PlistItem
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