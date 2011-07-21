{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module ArbitraryPList where

import Control.Applicative
import Data.Functor.Identity
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.PropertyList
import Data.PropertyList.Algebra
import Data.Time
import Test.QuickCheck

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> arbitrary
instance Arbitrary DiffTime where
    arbitrary = fromRational <$> arbitrary
instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary <*> arbitrary
instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (M.Map k v) where
    arbitrary = M.fromList <$> arbitrary

instance Arbitrary a => Arbitrary (PropertyListS a) where
    arbitrary = oneof
        [ PLArray   <$> shrinkingListOf arbitrary
        , PLData    <$> arbitrary
        , PLDate    <$> arbitrary
        , PLDict    <$> M.fromList <$> shrinkingListOf arbitrary
        , PLReal    <$> arbitrary
        , PLInt     <$> arbitrary
        , PLString  <$> arbitrary
        , PLBool    <$> arbitrary
        ]
        where
            shrinkingListOf xs = sized $ \sz -> do
                let lSz = max 1 (floor (sqrt (fromIntegral sz)))
                n     <- choose (1, lSz)
                sizes <- vectorOf n (choose (1, lSz))
                sequence [resize (s `div` sum sizes) xs | s <- sizes]
                

instance Arbitrary PropertyList where
    arbitrary = (plistAlgebra . Identity) <$> arbitrary

instance Arbitrary a => Arbitrary (PartialPropertyList a) where
    arbitrary = frequency
        [ (10, (plistAlgebra . Identity) <$> arbitrary)
        , (1,  pure <$> arbitrary)
        ]

instance CoArbitrary BS.ByteString where
    coarbitrary = coarbitrary . BS.unpack
instance CoArbitrary Day where
    coarbitrary (ModifiedJulianDay x) = coarbitrary x
instance CoArbitrary DiffTime where
    coarbitrary = coarbitrary . toRational
instance CoArbitrary UTCTime where
    coarbitrary (UTCTime d t) = coarbitrary d >< coarbitrary t
instance (CoArbitrary k, CoArbitrary v) => CoArbitrary (M.Map k v) where
    coarbitrary = coarbitrary . M.toAscList

instance CoArbitrary a => CoArbitrary (PropertyListS a) where
    coarbitrary (PLArray  x) = variant 0 . coarbitrary x
    coarbitrary (PLData   x) = variant 1 . coarbitrary x
    coarbitrary (PLDate   x) = variant 2 . coarbitrary x
    coarbitrary (PLDict   x) = variant 3 . coarbitrary x
    coarbitrary (PLReal   x) = variant 4 . coarbitrary x
    coarbitrary (PLInt    x) = variant 5 . coarbitrary x
    coarbitrary (PLString x) = variant 6 . coarbitrary x
    coarbitrary (PLBool   x) = variant 7 . coarbitrary x

instance CoArbitrary PropertyList where
    coarbitrary = coarbitrary . runIdentity . plistCoalgebra

instance CoArbitrary a => CoArbitrary (PartialPropertyList a) where
    coarbitrary 
        = coarbitrary 
        . (plistCoalgebra :: PartialPropertyList a -> Either a (PropertyListS (PartialPropertyList a)))
