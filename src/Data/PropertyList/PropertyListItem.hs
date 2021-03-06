{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    TemplateHaskell, CPP,
    ViewPatterns
  #-}

module Data.PropertyList.PropertyListItem
    (PropertyListItem(..)) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.PropertyList.Algebra
import Data.PropertyList.Types

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Time
import Data.Char
import Data.Int
import qualified Data.Text as Text
import Data.Word

import Data.OneOfN

import Control.Monad
import Data.Functor.Identity
import qualified Data.Traversable as Traversable
import Data.Generics

-- * some local utility functions

-- try to read a string to some other format using the Read instance
-- and failing gracefully
tryRead :: Read a => String -> Maybe a
tryRead s = case reads s of
    [(d, "")] -> Just d
    _ -> Nothing

-- try to convert a fractional type to an integral type without loss of precision.
-- in case of loss of precision, the conversion fails.
tryToIntegral :: (RealFrac a, Integral b) => a -> Maybe b
tryToIntegral d = case properFraction d of
    (i, 0) -> Just i
    _ -> Nothing

toStrictByteString :: BL.ByteString -> BS.ByteString
toStrictByteString = BS.concat . BL.toChunks
toLazyByteString :: BS.ByteString -> BL.ByteString
toLazyByteString = BL.fromChunks . return

-- |A class for items which can be converted to and from property lists.  This
-- is more general than 'PListAlgebra' and 'PListCoalgebra', in that it allows
-- for transformations that are not primitive-recursive.  This relaxation is
-- necessary and desirable in the 'PropertyListItem' situation because we are 
-- more interested in composable injection/projection operations on than in
-- universal maps.
--
-- The algebraic interface also cannot work for arrays or dictionaries,
-- because it only allows primitive (co-)recursion - the conversions can only
-- operate on one \"layer\" of 'PropertyListS' at a time.  This could be 
-- handled by enlarging the types (from [t] to Either t [t], for example)
-- or by encoding in-band (by taking a singleton list to be an element 
-- instead of a list, for example), but both of those \"solutions\" create
-- headaches of their own, and in any case the algebraic interface is probably
-- too bizarre for most users.
class PropertyListItem i where
    -- |Construct a 'PropertyList' from the item.
    toPropertyList :: i -> PropertyList
    
    -- |Convert a property list to a property list item if its contents
    -- _exactly_ fit the target type.  Note that when using types
    -- such as 'M.Map' 'String' 'Int' (as opposed to 'M.Map' 'String'
    -- 'PropertyList') this will mean that a single element of the 
    -- dictionary of a non-'Int' type will cause the entire conversion to
    -- fail.
    fromPropertyList :: PropertyList -> Maybe i
    
    -- |In order to support a general instance for lists without breaking
    -- String, we use the same trick as the Prelude uses for Show.
    -- Generally, the list methods should not be overridden, and maybe
    -- they shouldn't even be exported.
    listToPropertyList :: [i] -> PropertyList
    listToPropertyList      = plArray . map toPropertyList
    
    listFromPropertyList :: PropertyList -> Maybe [i]
    listFromPropertyList (fromPlArray -> Just x)    = mapM fromPropertyList x
    listFromPropertyList _                          = Nothing

instance PropertyListItem a => PropertyListItem [a] where
    toPropertyList = listToPropertyList
    fromPropertyList = listFromPropertyList

instance PropertyListItem PropertyList where
    toPropertyList = id
    fromPropertyList = Just

instance PropertyListItem BS.ByteString where
    toPropertyList = plData
    fromPropertyList (fromPlData    -> Just x) = Just x
--    fromPropertyList (fromPlString  -> Just x) = Just (toStrictByteString x)
    fromPropertyList _ = Nothing

instance PropertyListItem BL.ByteString where
    toPropertyList = plData . toStrictByteString
    fromPropertyList (fromPlData    -> Just x) = Just (toLazyByteString x)
--    fromPropertyList (fromPlString  -> Just x) = Just (toLazyByteString x)
    fromPropertyList _ = Nothing

instance PropertyListItem UTCTime where
    toPropertyList = plDate
    fromPropertyList (fromPlDate -> Just x) = Just x
    fromPropertyList _ = Nothing

instance PropertyListItem a => PropertyListItem (M.Map String a) where
    {-# SPECIALIZE instance PropertyListItem (M.Map String PropertyList) #-}
    
    toPropertyList = plDict . fmap toPropertyList
    fromPropertyList (fromPlDict -> Just x) = Traversable.mapM fromPropertyList x
    fromPropertyList _ = Nothing

instance PropertyListItem Double where
    toPropertyList = plReal
    fromPropertyList (fromPlInt    -> Just i) = Just (fromInteger i)
    fromPropertyList (fromPlReal   -> Just d) = Just d
    fromPropertyList (fromPlString -> Just s) = tryRead s
    fromPropertyList _ = Nothing

instance PropertyListItem Float where
    toPropertyList = toPropertyList . (realToFrac :: Float -> Double)
    fromPropertyList = fmap (realToFrac :: Double -> Float) . fromPropertyList

-- this little bit of Template Haskell replicates the embedded instance
-- to many other integral types (by using SYB generics to replace every
-- occurrence of ''Integer in the declarations' template-haskell representation
-- with each other type's name)
$( do
    decls <- [d|
            instance PropertyListItem Integer where
                toPropertyList = plInt . fromIntegral
                fromPropertyList pl = case runIdentity (plistCoalgebra pl) of 
                    PLInt  i    -> Just (fromIntegral i)
                    PLReal d    -> tryToIntegral d
                    PLString s  -> tryRead s
                    _           -> Nothing
        |]
    sequence
        [ everywhereM (mkM (return . replace)) dec
        | t <-  [''Integer, ''Int,
                 ''Int8,  ''Int16,  ''Int32,  ''Int64, 
                 ''Word8, ''Word16, ''Word32, ''Word64]
        , dec <- decls
        , let replace name
                | name  == ''Integer    = t
                | otherwise             = name
        ]
 )

-- this instance doesnt make much sense by itself, but must be here to support strings
instance PropertyListItem Char where
    toPropertyList c = plString [c]
    fromPropertyList (fromPlString -> Just  [c]) = Just c
    fromPropertyList _ = Nothing
    
    listToPropertyList = plString
    listFromPropertyList (fromPlString -> Just x)     = Just x
--    listFromPropertyList (fromPlData   -> Just x)     = Just (fromStrictByteString x)
    listFromPropertyList (fromPlBool   -> Just True)  = Just "YES"
    listFromPropertyList (fromPlBool   -> Just False) = Just "NO"
    listFromPropertyList (fromPlInt    -> Just i)     = Just (show i)
    listFromPropertyList (fromPlReal   -> Just d)     = Just (show d)
    listFromPropertyList other = Nothing

instance PropertyListItem Text.Text where
    toPropertyList = toPropertyList . Text.unpack
    fromPropertyList = fmap Text.pack . fromPropertyList

instance PropertyListItem Bool where
    toPropertyList = plBool
    fromPropertyList (fromPlBool   -> Just d) = Just d
    fromPropertyList (fromPlString -> Just b)
        | map toLower b `elem` ["yes", "true"]
        = Just True
        | map toLower b `elem` ["no", "false"]
        = Just False
    fromPropertyList _ = Nothing

-- The following TH generates, for Either and for all OneOfN types
--  (N in [2..20]), an instance of the form:
-- 
-- instance (PropertyListItem a, PropertyListItem b, PropertyListItem c) => PropertyListItem (OneOf3 a b c) where
--     toPropertyList = oneOf3 toPropertyList toPropertyList toPropertyList
--     fromPropertyList pl = msum [ fmap OneOf3 (fromPropertyList pl)
--                                , fmap TwoOf3 (fromPropertyList pl)
--                                , fmap ThreeOf3 (fromPropertyList pl)
--                                ]

$(  let types = ''Either : [mkTcName ("OneOf" ++ show n) | n <- [2..20]]
        -- mkTcName ensures we have the type constructor and not the data constructor
        -- by assembling it with its 'flavour' explicitly set to match that of a known
        -- type constructor.
        mkTcName n = Name (mkOccName n) nameFlavour
            where Name _ nameFlavour = ''OneOf2
        
        mkInstance typeName = do
            TyConI (DataD _ _ _ cons _) <- reify typeName
            let conNames = [name | NormalC name _ <- cons]
            
            let tyVarNames = zipWith (\con n -> mkName ("a" ++ show n)) conNames [1..]
                
                tyVars = map varT tyVarNames
                typeWithVars = foldl appT (conT typeName) tyVars
                
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 612
                context = cxt [classP ''PropertyListItem [tyVar]    | tyVar <- tyVars]
#else
                context = cxt [appT (conT ''PropertyListItem) tyVar | tyVar <- tyVars]
#endif
                inst = appT (conT ''PropertyListItem) typeWithVars
                
                pl = mkName "pl"
                
                whre = 
                    [ funD 'toPropertyList   [clause []        (normalB toPLbody  ) []]
                    , funD 'fromPropertyList [clause [varP pl] (normalB fromPLbody) []]
                    ]
                
                lcFirst (c:cs) = toLower c : cs
                fold  = varE . mkName . lcFirst . nameBase
                toPLbody = appsE (fold typeName : map (const (varE 'toPropertyList)) conNames)
                fromPLbody = appE (varE 'msum) $ listE
                    [ [| fmap $(conE con) (fromPropertyList $(varE pl)) |]
                    | con <- conNames
                    ]
        
            instanceD context inst whre
    in
    mapM mkInstance types
 )