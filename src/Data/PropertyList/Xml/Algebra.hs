{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.PropertyList.Xml.Algebra
    ( UnparsedXmlPlistItem(..)
    , unparsedXmlPlistItemToElement
    ) where

import qualified Data.ByteString as BS
import qualified Codec.Binary.Base64 as B64
import Data.Functor.Identity
import qualified Data.Map as M
import Data.PropertyList.Algebra
import Data.Time
import System.Locale
import Text.XML.Light

dateFormat :: String
dateFormat = "%FT%T%QZ"

instance PListAlgebra Identity Element where
    plistAlgebra = toElem . runIdentity
        where
            toElem :: PropertyListS Element -> Element
            toElem (PLArray    x) = unode "array" x
            toElem (PLData     x) = unode "data" (B64.encode (BS.unpack x))
            toElem (PLDate     x) = unode "date" (formatTime defaultTimeLocale dateFormat x)
            toElem (PLDict     x) = unode "dict" $ concat
                [ [ unode "key" k, v]
                | (k,v) <- M.toAscList x
                ]
            toElem (PLReal     x) = unode "real"    (show x)
            toElem (PLInt      x) = unode "integer" (show x)
            toElem (PLString   x) = unode "string"  x
            toElem (PLBool  True) = unode "true"    ()
            toElem (PLBool False) = unode "false"   ()

-- |A representation of values that were structurally sound in the 
-- property list file but the contents of which couldn't be interpreted
-- as what they claimed to be.  The result of the initial parse phase will
-- typically be a @PartialPropertyList UnparsedXmlPlistItem@, and if
-- the whole plist was parsed properly will contain no actual values 
-- of this type.
data UnparsedXmlPlistItem
    = UnparsedData String
    | UnparsedDate String
    | UnparsedInt  String
    | UnparsedReal String
    | UnparsedXml Element
    deriving Show

unparsedXmlPlistItemToElement = toElem
    where
        toElem (UnparsedData x) = unode "data"    x
        toElem (UnparsedDate x) = unode "date"    x
        toElem (UnparsedInt  x) = unode "integer" x
        toElem (UnparsedReal x) = unode "real"    x
        toElem (UnparsedXml  e) = e

instance PListAlgebra (Either Element) Element where
    plistAlgebra (Left x) = x
    plistAlgebra (Right x) = plistAlgebra (Identity x)

instance PListAlgebra (Either UnparsedXmlPlistItem) Element where
    plistAlgebra (Left x) = unparsedXmlPlistItemToElement x
    plistAlgebra (Right x) = plistAlgebra (Identity x)

instance PListCoalgebra (Either UnparsedXmlPlistItem) Element where
    -- I can't find any info anywhere about what namespace URI, if any, should
    -- be used for XML property lists.  So, ignoring it.
    plistCoalgebra e = coalg e
        where
            coalg (Element (QName name _ _) [] content _) = fromElem name content
            coalg _ = reject UnparsedXml e
            
            fromElem "array" content
                = accept PLArray (onlyElems content)
            fromElem "data" content
                = let contentText = text content
                   in case B64.decode contentText of
                        Just xs -> accept (PLData . BS.pack) xs
                        Nothing -> reject UnparsedData contentText
            fromElem "date" content
                = let contentText = text content
                   in case parseTime defaultTimeLocale dateFormat contentText of
                        Nothing -> reject UnparsedDate contentText
                        Just x  -> accept PLDate x
            fromElem "dict" content
                = fmap (PLDict . M.fromList) (fromDict (onlyElems content))
            fromElem "real" content
                = tryRead PLReal UnparsedReal (text content)
            fromElem "integer" content
                = tryRead PLInt UnparsedInt (text content)
            fromElem "string" content
                = accept PLString (text content)
            fromElem "true"  [] = accept PLBool True
            fromElem "false" [] = accept PLBool False
            fromElem _ _ = reject UnparsedXml e
            
            fromDict [] = Right []
            fromDict (key : value : rest)
                = case key of
                    Element (QName "key" _ _) [] content _
                        -> fmap ((text content, value) :) (fromDict rest)
                    _   -> reject UnparsedXml e
            
            text = concatMap cdData . onlyText
            
            accept :: (a -> c) -> a -> Either b c
            accept con = Right . con
            
            reject :: (a -> b) -> a -> Either b c
            reject con = Left  . con
            
            tryRead :: Read a => (a -> c) -> (String -> b) -> String -> Either b c
            tryRead onGood onBad str =
                    case reads str of
                            ((result, ""):_) -> accept onGood result
                            _                -> reject onBad  str

instance PListCoalgebra Maybe Element where
    plistCoalgebra
        = either (const Nothing :: UnparsedXmlPlistItem -> Maybe a) Just
        . plistCoalgebra
