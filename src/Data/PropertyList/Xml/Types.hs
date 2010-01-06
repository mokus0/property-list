{-# LANGUAGE
        TemplateHaskell, CPP
  #-}

module Data.PropertyList.Xml.Types
    ( Plist, PlistItem
    , plistItemToPlist, plistToPlistItem
    , readXmlPlist, showXmlPlist
    ) where

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

-- * The 'PlistItem' type: Xml-parser-independent view of an xml plist
-- |'PlistItem' is nearly equivalent to 'Plist' - the difference is that it discards
-- information about where in the tree the item is (a 'Plist' represents the whole
-- XML tree starting from the root).  This is a \"slightly-less-opaque\" type,
-- but still isn't really intended for consumption by end users.
type PlistItem = OneOf9 Array Data Date Dict AReal AInteger AString X.True X.False

-- |Convert a 'Plist' to a 'PlistItem', discarding the root element and any 
-- attributes that element may have had.
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

-- |Convert a 'PlistItem' to a 'Plist', giving it a root element with no 
-- attributes.
plistItemToPlist :: PlistItem -> Plist
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

-- * Parsing and rendering XML 'Plist's from/to 'String's

#ifdef HaXml_1_13

-- |Try to parse a string as an XML property list.
readXmlPlist :: String -> Either String Plist
readXmlPlist xml = case X2H.readXml xml of
    Nothing     -> Left "readXml: parse failed"
    Just plist  -> Right plist

-- |Render a 'Plist' to a 'String' as an XML property list.
showXmlPlist :: Plist -> String
showXmlPlist = X2H.showXml

-- |Render a 'Plist' as a 'Document' (HaXml's internal DOM representation, I
-- think)
toXml :: Plist -> Document
toXml value =
    Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing)) [] Nothing [])
             emptyST
             ( case (toElem value) of
                 [CElem e] -> e
                 )
             []

#else

-- |Try to parse a string as an XML property list.
readXmlPlist :: String -> Either String Plist
readXmlPlist = readXml

-- |Render a 'Plist' to a 'String' as an XML property list.
showXmlPlist :: Plist -> String
showXmlPlist x =
    case toContents x of
      [CElem _ _] -> (render . document . toXml) x
      _ -> ""

-- |Render a 'Plist' as a 'Document' (HaXml's internal DOM representation, I
-- think)
toXml :: Plist -> Document ()
toXml value =
    Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing)) [] Nothing [])
             emptyST
             ( case (toContents value) of
                 [CElem e ()] -> e
                 )
             []

#endif
