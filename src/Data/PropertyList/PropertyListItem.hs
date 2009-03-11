{-
 -      ``Data/PropertyList/PropertyListItem''
 -}
{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleInstances,
    GeneralizedNewtypeDeriving,
    TemplateHaskell
  #-}

module Data.PropertyList.PropertyListItem where

import Language.Haskell.TH
import Language.Haskell.TH.Fold

import Data.PropertyList.Type

import qualified Data.Map as M
import Data.ByteString (ByteString)
import Data.Time
import Data.Char

import Text.XML.HaXml.OneOfN

import Control.Monad
import Control.Monad.State

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
    return (fmap PLDict d')
tryAlterDictionaryEntryM k f (Just (PLDict d)) = do
    d' <- alterDictionaryEntryM k f (Just d)
    return (fmap PLDict d')
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
tryAlterDictionaryEntry k f Nothing           = (fmap PLDict (alterDictionaryEntry k f Nothing), True)
tryAlterDictionaryEntry k f (Just (PLDict d)) = (fmap PLDict (alterDictionaryEntry k f (Just d)), True)
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

-- can't make this polymorphic in the list element because
-- of overlap with 'String'
instance PropertyListItem [PropertyList] where
    toPropertyList = PLArray
    fromPropertyList (PLArray x) = Just x
    fromPropertyList _ = Nothing

-- |A newtype wrapper for lists, until (hopefully) we come up with a
-- way to allow an instance roughly equivalent to:
-- 
-- > instance PropertyListItem a => PropertyListItem [a]
-- 
-- without breaking the 'String' instance
newtype List x = List { unwrapList :: [x] }
    deriving (Eq, Ord, Show, Functor, Monad)

instance PropertyListItem a => PropertyListItem (List a) where
    toPropertyList = PLArray . map toPropertyList . unwrapList
    fromPropertyList (PLArray x) = fmap List (mapM fromPropertyList x)
    fromPropertyList _ = Nothing

instance PropertyListItem ByteString where
    toPropertyList = PLData
    fromPropertyList (PLData x) = Just x
    fromPropertyList _ = Nothing

instance PropertyListItem UTCTime where
    toPropertyList = PLDate
    fromPropertyList (PLDate x) = Just x
    fromPropertyList _ = Nothing

{-# SPECIALIZE instance PropertyListItem (M.Map String PropertyList) #-}

instance PropertyListItem a => PropertyListItem (M.Map String a) where
    toPropertyList = PLDict . fmap toPropertyList
    fromPropertyList (PLDict x) = fmapM fromPropertyList x
        where fmapM f m = fmap M.fromList $ 
                            sequence [ do { v <- f v; return (k, v)}
                                     | (k, v) <- M.toList m ]
    fromPropertyList _ = Nothing

instance PropertyListItem Double where
    toPropertyList = PLReal
    fromPropertyList (PLReal d) = Just d
    fromPropertyList (PLString s) = case reads s of
        [(d, "")] -> Just d
        _ -> Nothing
    fromPropertyList _ = Nothing

instance PropertyListItem Integer where
    toPropertyList = PLInt
    fromPropertyList (PLInt i) = Just i
    fromPropertyList (PLString s) = case reads s of
        [(i, "")] -> Just i
        _ -> Nothing
    fromPropertyList _ = Nothing

instance PropertyListItem String where
    toPropertyList = PLString
    fromPropertyList (PLString x) = Just x
    fromPropertyList (PLBool True) = Just "YES"
    fromPropertyList (PLBool False) = Just "NO"
    fromPropertyList (PLInt i) = Just (show i)
    fromPropertyList (PLReal d) = Just (show d)
    fromPropertyList _ = Nothing

instance PropertyListItem Bool where
    toPropertyList = PLBool
    fromPropertyList (PLBool d) = Just d
    fromPropertyList (PLString b)
        | map toLower b `elem` ["yes", "true"]
        = Just True
        | map toLower b `elem` ["no", "false"]
        = Just False
    fromPropertyList _ = Nothing

instance PropertyListItem UnparsedPlistItem where
    toPropertyList = PLVar
    fromPropertyList (PLVar d) = Just d
    fromPropertyList _ = Nothing

-- The following TH generates, for Either and for all OneOfN types
--  (N in [2..20]), an instance of the form:
-- 
-- instance (PropertyListItem a, PropertyListItem b, PropertyListItem c) => PropertyListItem (OneOf3 a b c) where
--     toPropertyList = $(fold ''OneOf3) toPropertyList toPropertyList toPropertyList
--     fromPropertyList pl = msum [ fmap OneOf3 (fromPropertyList pl)
--                                , fmap TwoOf3 (fromPropertyList pl)
--                                , fmap ThreeOf3 (fromPropertyList pl)
--                                ]

$(  let types = ''Either : map (mkName . ("OneOf" ++) . show) [2..20]
        
        mkInstance typeName = do
            let -- the mkName above selects the TyCon in some ghcs and the DataCon in others...
                -- there's probably an easier / cleaner way to sort that out, but I didn't bother looking for it.
                getTypeName (ForallT _ _ t) = getTypeName t
                getTypeName (VarT _)        = Nothing
                getTypeName (ConT n)        = Just n
                getTypeName (AppT x y)      = getTypeName x `mplus` getTypeName y
                getTypeName (ArrowT)        = Nothing
                getTypeName (ListT)         = Nothing
                getTypeName (TupleT n)      = Nothing
                
                getCons name = do
                    tyCon <- reify name
                    case tyCon of
                        DataConI _ t _ _ -> 
                            case getTypeName t of
                                Just name -> getCons name
                                Nothing -> fail $ unwords 
                                    [ "unexpected Type in DataConI"
                                    , show name, "-", show t
                                    ]
                        TyConI (DataD _ _ _ cons _) -> return (cons, name)
                        otherTycon -> fail $ unwords 
                            [ "unexpected result from reify"
                            , show name, "-", show otherTycon
                            ]

                
            
            (cons, typeName) <- getCons typeName
            let conNames = [name | NormalC name _ <- cons]
            
            tyVarNames <- zipWithM (\con n -> newName ("a" ++ show n)) conNames [1..]
        
            let tyVars = map varT tyVarNames
                typeWithVars = foldl appT (conT typeName) tyVars
                
                cxt = mapM (appT (conT ''PropertyListItem)) tyVars
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
        
            instanceD cxt inst whre
    in
    mapM mkInstance types
 )