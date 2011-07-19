{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.PropertyList.Binary.Algebra where

import Data.Monoid
import Control.Monad.Identity
import Data.PropertyList.Algebra
import Data.PropertyList.Binary.Types
import Data.Sequence as S ((<|), (><))
import qualified Data.Sequence as S
import qualified Data.Map as M

instance PListAlgebra Identity (BPListRecords Rel) where
    plistAlgebra = BPListRecords 0 . flatten . runIdentity
        where
            indexFrom n [] = []
            indexFrom n (BPListRecords root recs : rest)
                = n + fromIntegral root : indexFrom (n + fromIntegral (S.length recs)) rest
            
            flatten (PLArray  xss)
                =  BPLArray (indexFrom 1 xss)
                <| mconcat (map records xss)
            flatten (PLDict   kvs)
                =  BPLDict [1..nks] (indexFrom (nks+1) vss)
                <| S.fromList (map BPLString ks)
                >< mconcat (map records vss)
                where
                    nks = fromIntegral (M.size kvs)
                    ks  = M.keys  kvs
                    vss = M.elems kvs
            flatten (PLData  bs) = S.singleton (BPLData  bs)
            flatten (PLDate   t) = S.singleton (BPLDate   t)
            flatten (PLReal   r) = S.singleton (BPLReal   r)
            flatten (PLInt    i) = S.singleton (BPLInt    i)
            flatten (PLString s) = S.singleton (BPLString s)
            flatten (PLBool   b) = S.singleton (BPLBool   b)

instance PListCoalgebra (Either UnparsedBPListRecord) (BPListRecords Abs) where
    plistCoalgebra (BPListRecords root recs) = fmap (fmap (flip BPListRecords recs)) (unpackRec root)
        where
            unpackRec i
                | fromIntegral i >= S.length recs
                    = Left (MissingObjectRef i)
                | otherwise
                    = case S.index recs (fromIntegral i) of
                        BPLNull         -> Left UnparsedNull
                        BPLFill         -> Left UnparsedFill
                        BPLSet s        -> Left (UnparsedSet s)
                        BPLUID s        -> Left (UnparsedUID s)
                        BPLArray   xs   -> Right (PLArray xs)
                        BPLData     x   -> Right (PLData x)
                        BPLDate     x   -> Right (PLDate x)
                        BPLDict ks vs   -> do
                            ks <- sequence 
                                [ do
                                    key <- unpackRec k
                                    case key of
                                        PLString s -> Right s
                                        _ -> Left (UnparsedDict ks vs)
                                | k <-  ks
                                ]
                            return (PLDict (M.fromList (zip ks vs)))
                        BPLReal     x   -> Right (PLReal x)
                        BPLInt      x   -> Right (PLInt x)
                        BPLString   x   -> Right (PLString x)
                        BPLBool     x   -> Right (PLBool x)

-- To support smart-deconstructors:
instance PListCoalgebra Maybe (BPListRecords Abs) where
    plistCoalgebra 
        = either (const Nothing :: UnparsedBPListRecord -> Maybe t) Just
        . plistCoalgebra
