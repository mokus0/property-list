module Data.PropertyList.KeyPath 
    ( alterItemAtKeyPathM, alterItemAtKeyPath
    , getItemAtKeyPath, setItemAtKeyPath
    ) where

import Control.Monad
import Control.Monad.Trans.State
import Data.Functor.Identity
import qualified Data.Map as M
import Data.PropertyList.Algebra
import Data.PropertyList.PropertyListItem
import Data.PropertyList.Types (PropertyList)

-- |Alter a @'Maybe' 'PropertyList'@, viewing it as an instance of 'PropertyListItem'
-- and re-synthesizing it from a (possibly different) instance of 'PropertyListItem'.
-- Input and output are lifted by 'Maybe' in order to support adding or deleting of
-- items (as this function is used inside the key-path based versions)
--
-- Actually only needs 'Functor', not 'Monad' but since it's not exported
-- and is only used in 'Monad' contexts, I'm using 'Monad' to avoid needing
-- to add 'Functor' to those other contexts.
{-# INLINE alterPropertyListM #-}
alterPropertyListM ::
     (Monad m, PropertyListItem i, PropertyListItem i') 
     => (Maybe i -> m (Maybe i'))
     -> Maybe PropertyList -> m (Maybe PropertyList)
alterPropertyListM f = liftM (fmap toPropertyList) . f . (>>= fromPropertyList)

-- |Alter the contents of a dictionary (represented as a M.Map String), 
-- viewing its values through the 'PropertyListItem' class.
--
-- If the result of the alteration is 'Nothing', and the resulting dictionary
-- is empty, the result of the whole operation is 'Nothing' (causing cascading
-- deletion of empty key paths in 'alterDictionaryEntryM' et al.)
{-# INLINE alterDictionaryEntryM #-}
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
        Just i'' -> Just (M.insert k (toPropertyList i'') dict)

-- |Attempt to view a 'PropertyList' as a 'M.Map String' and, if successful,
-- modify an entry of the dictionary (by calling 'alterDictionaryEntryM').
-- If not possible, 'fail's.
--
-- If the result of the alteration is 'Nothing', and the resulting dictionary
-- is empty, that dictionary is deleted in the result.
{-# SPECIALIZE tryAlterDictionaryEntryM :: (PropertyListItem i, PropertyListItem i')
                                        => String 
                                        -> (Maybe i -> Identity (Maybe i'))
                                        -> Maybe PropertyList -> Identity (Maybe PropertyList)
  #-}
{-# SPECIALIZE tryAlterDictionaryEntryM :: (PropertyListItem i)
                                        => String 
                                        -> (Maybe i -> StateT (Maybe i) Maybe (Maybe i))
                                        -> Maybe PropertyList -> StateT (Maybe i) Maybe (Maybe PropertyList)
  #-}
tryAlterDictionaryEntryM ::
    (Monad m, PropertyListItem i, PropertyListItem i') 
    => String -> (Maybe i -> m (Maybe i'))
    -> Maybe PropertyList -> m (Maybe PropertyList)
tryAlterDictionaryEntryM k f mbPl = 
    case fmap fromPlDict mbPl of
        -- outer 'Maybe' is 'Just' if a plist was provided, 
        -- inner is 'Just' if that plist is a dictionary.
        Just (Just d)   -> alterDict (Just d)
        Nothing         -> alterDict Nothing
        Just Nothing    -> fail "Key path tries to pass through non-dictionary thing."
    where alterDict = liftM (fmap plDict) . alterDictionaryEntryM k f

-- |@alterItemAtKeyPathM path f@ applies the function @f@ deep inside the 
-- 'PropertyList' on the property list item at the given key-path @path@
-- (if possible).  This is the same notion of key path as is used in the 
-- Apple plist APIs - each component of the path indicates descending 
-- into a dictionary by selecting the element with that key (if any).  If a 
-- key is not found, it is created.  If a key is found but is not a 
-- dictionary, the operation fails (with 'fail' from the 'Monad' class).
-- 
-- If the result of @f@ is 'Nothing', and the resulting dictionary is empty,
-- that dictionary is deleted in the result (and any empty parent dictionaries).
-- If this is not the behavior you want, you should alter the parent dictionary 
-- itself and return an empty one.
{-# SPECIALIZE alterItemAtKeyPathM :: (PropertyListItem i, PropertyListItem i')
                                   => [String] 
                                   -> (Maybe i -> Identity (Maybe i'))
                                   -> Maybe PropertyList -> Identity (Maybe PropertyList)
  #-}
{-# SPECIALIZE alterItemAtKeyPathM :: (PropertyListItem i)
                                   => [String] 
                                   -> (Maybe i -> StateT (Maybe i) Maybe (Maybe i))
                                   -> Maybe PropertyList -> StateT (Maybe i) Maybe (Maybe PropertyList)
  #-}
alterItemAtKeyPathM ::
    (Monad m, PropertyListItem i, PropertyListItem i')
    => [String] -> (Maybe i -> m (Maybe i'))
    -> Maybe PropertyList -> m (Maybe PropertyList)
alterItemAtKeyPathM [] f = alterPropertyListM f
alterItemAtKeyPathM (k:ks) f = tryAlterDictionaryEntryM k (alterItemAtKeyPathM ks f)

-- |@alterItemAtKeyPath path f@ applies the function @f@ deep inside the 
-- 'PropertyList' on the property list item at the given key-path @path@
-- (if possible).  This is the same notion of key path as is used in the 
-- Apple plist APIs - namely, each component of the path indicates descending 
-- into a dictionary by selecting the element with that key (if any).  If a 
-- key is not found, it is created.  If a key is found but is not a 
-- dictionary, the operation fails (with 'error').
-- 
-- If the result of @f@ is 'Nothing', and the resulting dictionary is empty,
-- that dictionary is deleted in the result (and any empty parent dictionaries).
-- If this is not the behavior you want, you should alter the parent dictionary 
-- itself and return an empty one.
alterItemAtKeyPath :: 
    (PropertyListItem i, PropertyListItem i')
    => [String] -> (Maybe i -> Maybe i')
    -> Maybe PropertyList -> Maybe PropertyList
alterItemAtKeyPath path f = runIdentity . alterItemAtKeyPathM path (Identity . f)

-- |Gets the item, if any (and if convertible to the required type), 
-- at a given key path.  If the key path passes through something that 
-- is not a dictionary, the operation returns 'Nothing'.
getItemAtKeyPath :: PropertyListItem i =>
    [String] -> Maybe PropertyList -> Maybe i
    -- works by running a StateT (Maybe i) Maybe operation at the keypath.
    -- The operation captures the value it is passed and 'put's it into the
    -- state.  The result is the final state of the operation 'join'ed to
    -- the success/failure in the underlying 'Maybe' monad (which is why
    -- we use 'StateT' instead of just 'State' - the latter would cause
    -- failure by 'error' since it's equivalent to @'StateT' s 'Identity'@).
getItemAtKeyPath path plist = join $ execStateT 
    (alterItemAtKeyPathM path (\e -> put e >> return e) plist)
    Nothing

-- |Sets the item at a given key-path.  If the key path does not exist, it is
-- created.  If it exists but passes through something that is not a dictionary,
-- the operation fails (with 'error')
setItemAtKeyPath :: PropertyListItem i =>
    [String] -> Maybe i -> Maybe PropertyList -> Maybe PropertyList
setItemAtKeyPath path value plist = alterItemAtKeyPath path 
    (\e -> value `asTypeOf` e) plist

