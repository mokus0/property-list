{-
 -      ``Data/PropertyList/PropertyListItem''
 -      (c) 2008 James Cook
 -}
{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleInstances,
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

class PropertyListItem i where
    -- |Convert the item to a property list, usually by simply wrapping the
    -- 
    toPropertyList :: i -> PropertyList
    
    -- |Convert a property list to a property list item if its contents
    -- _exactly_ fit the target type.  Note that when using types
    -- such as 'M.Map' 'String' 'Int' (as opposed to 'M.Map' 'String'
    -- 'PropertyList') this will mean that a single element of the 
    -- dictionary of a non-'Int' type will cause the entire conversion to
    -- fail.
    fromPropertyList :: PropertyList -> Maybe i

-- this stuff is pretty sketchy at present.  It works, but I don't think I
-- really like it.  The types and semantics just don't feel right yet.
alterPropertyListIfPossible ::
    (PropertyListItem i, PropertyListItem i') 
    => (i -> (Maybe i', a))
    -> PropertyList -> (Maybe PropertyList, Maybe a)
alterPropertyListIfPossible f pl = case fmap f (fromPropertyList pl) of
    Nothing -> (Just pl, Nothing)
    Just (i', a) -> (fmap toPropertyList i', Just a)

alterItemAtKeyPath ::
    (PropertyListItem i, PropertyListItem i') 
    => [String] -> (i -> (Maybe i', a))
    -> PropertyList -> (Maybe PropertyList, Maybe a)
alterItemAtKeyPath []     f = alterPropertyListIfPossible f
alterItemAtKeyPath (k:ks) f = alterPropertyListIfPossible' 
    (alterDictionaryEntryIfExists' k
        (alterItemAtKeyPath ks f))
    where
        alterPropertyListIfPossible' ::
            (PropertyListItem i, PropertyListItem i') 
            => (i -> (i', Maybe a))
            -> PropertyList -> (Maybe PropertyList, Maybe a)
        alterPropertyListIfPossible' f pl = case fmap f (fromPropertyList pl) of
            Nothing -> (Just pl, Nothing)
            Just (i', a) -> (Just (toPropertyList i'), a)
        
        alterDictionaryEntryIfExists' ::
            (PropertyListItem i, PropertyListItem i') 
            => String -> (i -> (Maybe i', Maybe a)) 
            -> M.Map String PropertyList -> (M.Map String PropertyList, Maybe a)
        alterDictionaryEntryIfExists' k f dict = case M.lookup k dict >>= fromPropertyList of
            Nothing -> (dict, Nothing)
            Just item -> case f item of
                (item, a) -> (M.update (const (fmap toPropertyList item)) k dict, a)

getPropertyListItemAtKeyPath :: PropertyListItem i =>
    [String] -> PropertyList -> Maybe i
getPropertyListItemAtKeyPath path = snd . alterItemAtKeyPath path (\i -> (Just i,i))

instance PropertyListItem PropertyList where
    toPropertyList = id
    fromPropertyList = Just

-- can't make this polymorphic in the list element because
-- of overlap with 'String'
instance PropertyListItem [PropertyList] where
    toPropertyList = PLArray
    fromPropertyList (PLArray x) = Just x
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
            TyConI (DataD _ _ _ cons _) <- reify typeName
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