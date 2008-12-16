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

alterPropertyList :: 
    (PropertyListItem i, PropertyListItem i') 
    => (Maybe i -> (Maybe i', a))
    -> PropertyList -> (PropertyList, a)
alterPropertyList f plist = case f (fromPropertyList plist) of
    (plist', a) -> (maybe plist toPropertyList plist', a)

alterPropertyListIfPossible ::
    (PropertyListItem i, PropertyListItem i') 
    => (i -> (i', Maybe a))
    -> PropertyList -> (PropertyList, Maybe a)
alterPropertyListIfPossible f pl = case fmap f (fromPropertyList pl) of
    Nothing -> (pl, Nothing)
    Just (i', a) -> (toPropertyList i', a)

alterDictionaryEntry ::
    (PropertyListItem i, PropertyListItem i') 
    => String -> (Maybe i -> (Maybe i', Maybe a)) 
    -> M.Map String PropertyList -> (M.Map String PropertyList, Maybe a)
alterDictionaryEntry k f dict = case f (M.lookup k dict >>= fromPropertyList) of
    (entry', a) -> (maybe dict (\e -> M.insert k (toPropertyList e) dict) entry', a)

alterDictionaryEntryIfExists ::
    (PropertyListItem i, PropertyListItem i') 
    => String -> (i -> (i', Maybe a)) 
    -> M.Map String PropertyList -> (M.Map String PropertyList, Maybe a)
alterDictionaryEntryIfExists k f dict = case M.lookup k dict >>= fromPropertyList of
    Nothing -> (dict, Nothing)
    Just item -> case f item of
        (item, a) -> (M.insert k (toPropertyList item) dict, a)
    
alterItemAtKeyPath ::
    (PropertyListItem i, PropertyListItem i') 
    => [String] -> (i -> (i', Maybe a))
    -> PropertyList -> (PropertyList, Maybe a)
alterItemAtKeyPath []     f = alterPropertyListIfPossible f
alterItemAtKeyPath (k:ks) f = alterPropertyListIfPossible 
    $ alterDictionaryEntryIfExists k
    $ alterItemAtKeyPath ks f
    

instance PropertyListItem PropertyList where
    toPropertyList = id
    fromPropertyList = Just

{-# SPECIALIZE instance PropertyListItem [PropertyList] #-}

instance PropertyListItem a => PropertyListItem [a] where
    toPropertyList = PLArray . fmap toPropertyList
    fromPropertyList (PLArray x) = mapM fromPropertyList x
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
    fromPropertyList _ = Nothing

instance PropertyListItem Integer where
    toPropertyList = PLInt
    fromPropertyList (PLInt i) = Just i
    fromPropertyList _ = Nothing

instance PropertyListItem String where
    toPropertyList = PLString
    fromPropertyList (PLString x) = Just x
    fromPropertyList _ = Nothing

instance PropertyListItem Bool where
    toPropertyList = PLBool
    fromPropertyList (PLBool d) = Just d
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