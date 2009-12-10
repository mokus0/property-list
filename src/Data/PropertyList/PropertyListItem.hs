{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleInstances,
    GeneralizedNewtypeDeriving,
    TemplateHaskell, CPP
  #-}

module Data.PropertyList.PropertyListItem where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Fold

import Data.PropertyList.Type

import qualified Data.Map as M
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.ByteString.Class
import Data.Time
import Data.Char

import Text.XML.HaXml.OneOfN

import Control.Monad
import Control.Monad.State

import Data.Object

-- |A class for items which can be converted to and from property lists
class PropertyListItem i where
    -- |Convert the item to a property list, usually by simply wrapping the
    -- value with the appropriate 'PropertyList_' constructor
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
    listFromPropertyList (S (PLArray x))    = mapM fromPropertyList x
    listFromPropertyList _ = Nothing

instance PropertyListItem a => PropertyListItem [a] where
    toPropertyList = listToPropertyList
    fromPropertyList = listFromPropertyList

{-# SPECIALIZE alterPropertyListM :: Monad m => (Maybe (M.Map String PropertyList) -> m (Maybe (M.Map String PropertyList)))
                    -> Maybe PropertyList -> m (Maybe PropertyList) #-}
alterPropertyListM ::
    (Monad m, PropertyListItem i, PropertyListItem i') 
    => (Maybe i -> m (Maybe i'))
    -> Maybe PropertyList -> m (Maybe PropertyList)
alterPropertyListM f plist = do
    i' <- f (plist >>= fromPropertyList)
    return (fmap toPropertyList i')

{-# SPECIALIZE alterDictionaryEntryM :: Monad m => String -> (Maybe PropertyList -> m (Maybe PropertyList))
                    -> Maybe (M.Map String PropertyList) -> m (Maybe (M.Map String PropertyList)) #-}
alterDictionaryEntryM ::
    (Monad m, PropertyListItem i, PropertyListItem i') 
    => String -> (Maybe i -> m (Maybe i'))
    -> Maybe (M.Map String PropertyList) -> m (Maybe (M.Map String PropertyList))
alterDictionaryEntryM k f Nothing = do
    i' <- f Nothing
    return (fmap (M.singleton k . toPropertyList) i')
alterDictionaryEntryM k f (Just dict) = do
    let (dict', i) = case M.splitLookup k dict of
            (pre, v, post) -> (M.union pre post, fromPropertyList =<< v)
    
    i' <- f i
    return $ case i' of
        Nothing
            | M.null dict'  -> Nothing
            | otherwise     -> Just dict'
        Just i' -> Just (M.insert k (toPropertyList i') dict)
        
tryAlterDictionaryEntryM ::
    (Monad m, PropertyListItem i, PropertyListItem i') 
    => String -> (Maybe i -> m (Maybe i'))
    -> Maybe PropertyList -> m (Maybe PropertyList)
tryAlterDictionaryEntryM k f Nothing = do
    d' <- alterDictionaryEntryM k f Nothing
    return (fmap plDict d')
tryAlterDictionaryEntryM k f (Just (S (PLDict d))) = do
    d' <- alterDictionaryEntryM k f (Just d)
    return (fmap plDict d')
tryAlterDictionaryEntryM k f other = fail "Key path tries to pass through non-dictionary thing."

alterItemAtKeyPathM ::
    (Monad m, PropertyListItem i, PropertyListItem i')
    => [String] -> (Maybe i -> m (Maybe i'))
    -> Maybe PropertyList -> m (Maybe PropertyList)
alterItemAtKeyPathM [] f = alterPropertyListM f
alterItemAtKeyPathM (k:ks) f = tryAlterDictionaryEntryM k (alterItemAtKeyPathM ks f)

alterPropertyList ::
    (PropertyListItem i, PropertyListItem i') 
    => (Maybe i -> Maybe i')
    -> Maybe PropertyList -> Maybe PropertyList
alterPropertyList f plist = fmap toPropertyList (f (fromPropertyList =<< plist))

alterDictionaryEntry ::
    (PropertyListItem i, PropertyListItem i') 
    => String -> (Maybe i -> Maybe i')
    -> Maybe (M.Map String PropertyList) -> Maybe (M.Map String PropertyList)
alterDictionaryEntry k f Nothing = fmap (M.singleton k . toPropertyList) (f Nothing)
alterDictionaryEntry k f (Just dict) = case i' of
    Nothing
        | M.null dict'  -> Nothing
        | otherwise     -> Just dict'
    Just i' -> Just (M.insert k (toPropertyList i') dict)
    
    where
        (dict', i) = case M.splitLookup k dict of
            (pre, v, post) -> (M.union pre post, fromPropertyList =<< v)
        i' = f i

tryAlterDictionaryEntry ::
    (PropertyListItem i, PropertyListItem i') 
    => String -> (Maybe i -> Maybe i')
    -> Maybe PropertyList -> (Maybe PropertyList, Bool)
tryAlterDictionaryEntry k f Nothing           = (fmap plDict (alterDictionaryEntry k f Nothing), True)
tryAlterDictionaryEntry k f (Just (S (PLDict d))) = (fmap plDict (alterDictionaryEntry k f (Just d)), True)
tryAlterDictionaryEntry k f other = (other, False)

-- |TODO: capture the success/failure of the operation?
-- (can fail if key path tries to enter something that isn't a dictionary)
alterItemAtKeyPath :: 
    (PropertyListItem i, PropertyListItem i')
    => [String] -> (Maybe i -> Maybe i')
    -> Maybe PropertyList -> Maybe PropertyList
alterItemAtKeyPath  []    f plist= alterPropertyList f plist
alterItemAtKeyPath (k:ks) f plist = case tryAlterDictionaryEntry k (alterItemAtKeyPath ks f) plist of
    (_, False) -> error "Key path tries to pass through non-dictionary thing."
    (plist, True) -> plist

getItemAtKeyPath :: PropertyListItem i =>
    [String] -> Maybe PropertyList -> Maybe i
getItemAtKeyPath path plist = execState 
    (alterItemAtKeyPathM path (\e -> put e >> return e) plist)
    Nothing

setItemAtKeyPath :: PropertyListItem i =>
    [String] -> Maybe i -> Maybe PropertyList -> Maybe PropertyList
setItemAtKeyPath path value plist = alterItemAtKeyPath path 
    (\e -> value `asTypeOf` e) plist

instance PropertyListItem PropertyList where
    toPropertyList = id
    fromPropertyList = Just

instance PropertyListItem ByteString where
    toPropertyList = plData
    fromPropertyList (S (PLData x))   = Just x
    fromPropertyList (S (PLString x)) = Just (toStrictByteString x)
    fromPropertyList _ = Nothing

instance PropertyListItem Lazy.ByteString where
    toPropertyList = plData . toStrictByteString
    fromPropertyList (S (PLData   x)) = Just (toLazyByteString x)
    fromPropertyList (S (PLString x)) = Just (toLazyByteString x)
    fromPropertyList _ = Nothing

instance PropertyListItem UTCTime where
    toPropertyList = plDate
    fromPropertyList (S (PLDate x)) = Just x
    fromPropertyList _ = Nothing

instance PropertyListItem a => PropertyListItem (M.Map String a) where
    {-# SPECIALIZE instance PropertyListItem (M.Map String PropertyList) #-}
    
    toPropertyList = plDict . fmap toPropertyList
    fromPropertyList (S (PLDict x)) = fmapM fromPropertyList x
        where fmapM f m = fmap M.fromList $ 
                            sequence [ do { v <- f v; return (k, v)}
                                     | (k, v) <- M.toList m ]
    fromPropertyList _ = Nothing

instance PropertyListItem Double where
    toPropertyList = plReal
    fromPropertyList (S (PLInt i)) =  Just (fromInteger i)
    fromPropertyList (S (PLReal d)) = Just d
    fromPropertyList (S (PLString s)) = case reads s of
        [(d, "")] -> Just d
        _ -> Nothing
    fromPropertyList _ = Nothing

instance PropertyListItem Float where
    toPropertyList = toPropertyList . (realToFrac :: Float -> Double)
    fromPropertyList = fmap (realToFrac :: Double -> Float) . fromPropertyList

instance PropertyListItem Integer where
    toPropertyList = plInt
    fromPropertyList (S (PLInt i)) = Just i
    fromPropertyList (S (PLReal d)) = case properFraction d of
        (i, 0) -> Just i
        _ -> Nothing
    fromPropertyList (S (PLString s)) = case reads s of
        [(i, "")] -> Just i
        _ -> Nothing
    fromPropertyList _ = Nothing

instance PropertyListItem Int where
    toPropertyList = toPropertyList . toInteger
    fromPropertyList = fmap fromInteger . fromPropertyList

-- this instance doesnt make much sense by itself, but must be here to support strings
instance PropertyListItem Char where
    toPropertyList c = plString [c]
    fromPropertyList (S (PLString [c])) = Just c
    fromPropertyList _ = Nothing
    
    listToPropertyList = plString
    listFromPropertyList (S (PLString x)) = Just x
    listFromPropertyList (S (PLData x))   = Just (fromStrictByteString x)
    listFromPropertyList (S (PLBool True)) = Just "YES"
    listFromPropertyList (S (PLBool False)) = Just "NO"
    listFromPropertyList (S (PLInt i)) = Just (show i)
    listFromPropertyList (S (PLReal d)) = Just (show d)
    listFromPropertyList other = Nothing

instance PropertyListItem Bool where
    toPropertyList = plBool
    fromPropertyList (S (PLBool d)) = Just d
    fromPropertyList (S (PLString b))
        | map toLower b `elem` ["yes", "true"]
        = Just True
        | map toLower b `elem` ["no", "false"]
        = Just False
    fromPropertyList _ = Nothing

instance PropertyListItem UnparsedPlistItem where
    toPropertyList = plVar
    fromPropertyList (V d) = Just d
    fromPropertyList _ = Nothing

-- |Note that due to 'Object''s use of 'Lazy.ByteString's, scalars become data, not strings.
-- Using @GenObject key String@ instead of 'Object' will cause scalars to be encoded as plist strings.
#ifdef NEW_DATA_OBJECT
instance (PropertyListItem val, LazyByteString key) => PropertyListItem (GenObject key val) where
#else
instance PropertyListItem Object where
#endif
    toPropertyList (Mapping  m) = toPropertyList (M.fromList [(toStr k, v) | (k,v) <- m])
        where toStr = unpack . toLazyByteString
    toPropertyList (Sequence s) = toPropertyList s
    toPropertyList (Scalar   s) = toPropertyList s
    
    fromPropertyList plist = msum
        [ do
            m <- fromPropertyList plist
            let fromStr = fromLazyByteString . pack
            return (Mapping [(fromStr k, v) | (k,v) <- M.assocs m])
        , fmap Sequence (fromPropertyList plist)
        , fmap Scalar   (fromPropertyList plist)
        ]

-- The following TH generates, for Either and for all OneOfN types
--  (N in [2..20]), an instance of the form:
-- 
-- instance (PropertyListItem a, PropertyListItem b, PropertyListItem c) => PropertyListItem (OneOf3 a b c) where
--     toPropertyList = $(fold ''OneOf3) toPropertyList toPropertyList toPropertyList
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
                
#ifdef NEW_TEMPLATE_HASKELL
                preds = [classP ''PropertyListItem [tyVar] | tyVar <- tyVars]
                context = cxt preds
#else
                context = mapM (appT (conT ''PropertyListItem)) tyVars
#endif
                inst = appT (conT ''PropertyListItem) typeWithVars
                
                pl = mkName "pl"
                
                whre = 
                    [ funD 'toPropertyList   [clause []        (normalB toPLbody  ) []]
                    , funD 'fromPropertyList [clause [varP pl] (normalB fromPLbody) []]
                    ]
                
                toPLbody = appsE (fold typeName : map (const (varE 'toPropertyList)) conNames)
                fromPLbody = appE (varE 'msum) $ listE
                    [ [| fmap $(conE con) (fromPropertyList $(varE pl)) |]
                    | con <- conNames
                    ]
        
            instanceD context inst whre
    in
    mapM mkInstance types
 )