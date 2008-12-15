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
import Language.Haskell.TH.Fold

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