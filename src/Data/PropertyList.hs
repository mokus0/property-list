{-
 -      ``Data/PropertyList''
 -      rough cut type/reader/writer for Apple Property List format
 -}
{-# LANGUAGE
        TemplateHaskell
  #-}

module Data.PropertyList where

import Prelude as P
import Data.PropertyList.Xml.Dtd as X
import Data.PropertyList.Xml

import qualified Data.Map as M
import Data.ByteString as B hiding (map)
import Data.Time
import System.Locale
import Codec.Binary.Base64 as B64

import Text.XML.HaXml.OneOfN
import Util.TH.Fold

import Text.XML.HaXml.XmlContent

readPropertyListFromFile :: FilePath -> IO (Either String (PropertyList UnparsedPlistItem))
readPropertyListFromFile file = do
        x <- readPlistFromFile file
        return (fmap plistToPropertyList x)

writePropertyListToFile :: FilePath -> PropertyList UnparsedPlistItem -> IO ()
writePropertyListToFile file plist = do
        writePlistToFile file (propertyListToPlist unparsedPlistItemToPlistItem plist)

data PropertyList a
        = PLArray [PropertyList a]
        | PLDict  (M.Map String (PropertyList a))
        
        | PLBool Bool
        | PLData ByteString
        | PLDate UTCTime
        | PLNum Integer
        | PLReal Double
        | PLString String
        
        | PLVar a
        deriving (Eq, Ord, Show, Read)

instance Functor PropertyList where
        fmap f = substPropertyList (PLVar . f)

instance Monad PropertyList where
        return = PLVar
        (>>=) = flip substPropertyList

substPropertyList :: (a -> PropertyList b)
                     -> PropertyList a
                     -> PropertyList b
substPropertyList plVar = foldPropertyList
        PLArray
        PLDict
        PLBool
        PLData
        PLDate
        PLNum
        PLReal
        PLString
        plVar

-- TODO: if possible, make 'fold' in th-utils detect the Functor instances for
-- [] and Map k and automagically generate the call to fmap here
foldPropertyList :: ([a] -> a)
                 -> (M.Map String a -> a)
                 -> (Bool -> a)
                 -> (ByteString -> a)
                 -> (UTCTime -> a)
                 -> (Integer -> a)
                 -> (Double -> a)
                 -> (String -> a)
                 -> (t -> a)
                 -> PropertyList t -> a
foldPropertyList foldList foldMap a b c d e f g = go
        where
                go = $(fold ''PropertyList) foldArray foldDict a b c d e f g
                
                foldArray branches = foldList (fmap go branches)
                foldDict dict = foldMap (fmap go dict)

data UnparsedPlistItem
        = UnparsedData String
        | UnparsedDate String
        | UnparsedInteger String
        | UnparsedReal String
        deriving (Eq, Ord, Show, Read)

unparsedPlistItemToPlistItem :: UnparsedPlistItem -> PlistItem
unparsedPlistItemToPlistItem = $(fold ''UnparsedPlistItem)
        (TwoOf9 . Data)
        (ThreeOf9 . Date)
        (SixOf9 . AInteger)
        (FiveOf9 . AReal)


tryRead :: Read a => (a -> b) -> (String -> b) -> String -> b
tryRead onGood onBad str =
        case reads str of
                ((result, ""):_) -> onGood result
                _ -> onBad str

dateFormat :: String
dateFormat = "%FT%TZ"

plistToPropertyList :: Plist -> PropertyList UnparsedPlistItem
plistToPropertyList = plistItemToPropertyList . plistToPlistItem

plistItemToPropertyList :: PlistItem -> PropertyList UnparsedPlistItem
plistItemToPropertyList = $(fold ''OneOf9) 
        arrayToPLArray
        dataToPLData
        dateToPLDate
        dictToPLDict
        realToPLReal
        integerToPLNum
        stringToPLString
        trueToPLBool
        falseToPLBool
        
        where
                arrayToPLArray   (Array x) 
                        = PLArray  (map plistItemToPropertyList x)
                dataToPLData     (Data x) 
                        = case decode x of 
                                Just d -> PLData (pack d)
                                Nothing -> PLVar (UnparsedData x)
                dateToPLDate     (Date x)
                        = case parseTime defaultTimeLocale dateFormat x of
                                Just t -> PLDate t
                                Nothing -> PLVar (UnparsedDate x)
                dictToPLDict     (Dict x)
                        = PLDict (M.fromList [ (k, plistItemToPropertyList v) | Dict_ (Key k) v <- x])
                realToPLReal     (AReal x) 
                        = tryRead PLReal (PLVar . UnparsedReal) x
                integerToPLNum   (AInteger x) 
                        = tryRead PLNum (PLVar . UnparsedInteger) x
                stringToPLString (AString x) 
                        = PLString x
                trueToPLBool     x = PLBool P.True
                falseToPLBool    x = PLBool P.False

propertyListToPlist :: (a -> PlistItem) -> PropertyList a -> Plist
propertyListToPlist fromOther = plistItemToPlist (fromAttrs []) . propertyListToPlistItem fromOther

propertyListToPlistItem :: (a -> PlistItem) -> PropertyList a -> PlistItem
propertyListToPlistItem fromOther = foldPropertyList
        fromPLArray
        fromPLDict
        fromPLBool
        fromPLData
        fromPLDate
        fromPLNum
        fromPLReal
        fromPLString
        fromOther

        where
                fromPLArray  x = OneOf9 (Array x)
                fromPLData   x = TwoOf9 (Data (encode (unpack x)))
                fromPLDate   x = ThreeOf9 (Date (formatTime defaultTimeLocale dateFormat x))
                fromPLDict   x = FourOf9 (Dict [Dict_ (Key k) v | (k,v) <- M.toList x])
                fromPLReal   x = FiveOf9 (AReal (show x))
                fromPLNum    x = SixOf9 (AInteger (show x))
                fromPLString x = SevenOf9 (AString x)
                fromPLBool   P.True 
                               = EightOf9 X.True
                fromPLBool   P.False
                               = NineOf9 X.False
