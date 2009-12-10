{-# LANGUAGE
        TemplateHaskell, CPP
  #-}

module Data.PropertyList.Xml where

import Prelude as P

import Text.XML.HaXml.OneOfN

#ifdef HaXml_1_13
import Data.PropertyList.Xml.Dtd_1_13 as X
import Text.XML.HaXml.Xml2Haskell hiding (showXml, readXml)
import qualified Text.XML.HaXml.Xml2Haskell as X2H
#else
import Data.PropertyList.Xml.Dtd as X
import Text.XML.HaXml.XmlContent
        hiding (showXml, toXml)
#endif

import Text.PrettyPrint.HughesPJ (render)
import Text.XML.HaXml.Pretty   (document)
import Text.XML.HaXml.Types

import Language.Haskell.TH.Fold

type PlistItem = OneOf9 Array Data Date Dict AReal AInteger AString X.True X.False

readPlist :: String -> Either String Plist
readPlist = readXml

readPlistFromFile :: FilePath -> IO (Either String Plist)
readPlistFromFile path = do
        contents <- readFile path
        return (readXml contents)

writePlistToFile :: FilePath -> Plist -> IO ()
writePlistToFile path plist = do
        writeFile path (showXml plist)

#ifdef HaXml_1_13

readXml :: String -> Either String Plist
readXml xml = case X2H.readXml xml of
    Nothing     -> Left "readXml: parse failed"
    Just plist  -> Right plist

showXml :: Plist -> String
showXml = X2H.showXml

toXml :: Plist -> Document
toXml value =
    Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing)) [] Nothing [])
             emptyST
             ( case (toElem value) of
                 [CElem e] -> e
                 )
             []

#else

-- | Convert a fully-typed XML document to a string (without DTD).
showXml :: Plist -> String
showXml x =
    case toContents x of
      [CElem _ _] -> (render . document . toXml) x
      _ -> ""

toXml :: Plist -> Document ()
toXml value =
    Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing)) [] Nothing [])
             emptyST
             ( case (toContents value) of
                 [CElem e ()] -> e
                 )
             []

#endif

plistToPlistItem :: Plist -> PlistItem
plistToPlistItem = $(fold ''Plist)
        (\attr -> OneOf9  )
        (\attr -> TwoOf9  )
        (\attr -> ThreeOf9)
        (\attr -> FourOf9 )
        (\attr -> FiveOf9 )
        (\attr -> SixOf9  )
        (\attr -> SevenOf9)
        (\attr -> EightOf9)
        (\attr -> NineOf9 )

plistItemToPlist = $(fold ''OneOf9)
        (PlistArray    attr)
        (PlistData     attr)
        (PlistDate     attr)
        (PlistDict     attr)
        (PlistAReal    attr)
        (PlistAInteger attr)
        (PlistAString  attr)
        (PlistTrue     attr)
        (PlistFalse    attr)
        where attr = fromAttrs []