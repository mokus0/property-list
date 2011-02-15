{-# LANGUAGE 
    MultiParamTypeClasses,
    FlexibleContexts, FlexibleInstances, IncoherentInstances,
    GeneralizedNewtypeDeriving
  #-}

-- |This module implements the 'PropertyList' and 'PartialPropertyList' types
-- and their algebras/coalgebras.  These types are the core of the property-list
-- implementation, representing either complete or partial propertylists, 
-- respectively, in the most \"universal\" form possible.
module Data.PropertyList.Types where

import Data.PropertyList.Algebra

import Control.Applicative      (Applicative(..), WrappedMonad(..), (<$>))
import Control.Functor.Fix      (FixF(..))
import Control.Functor.Pointed  (Pointed(..), Copointed(..))
import Control.Monad            (liftM, ap)
import Control.Monad.Free       (Free(..), MonadFree(..), runFree)
import Control.Monad.Identity   (Identity(..))
import Data.Foldable            (Foldable(foldMap))
import Data.Traversable         (Traversable(..))
import Data.Void                (Void, void)

import Unsafe.Coerce            (unsafeCoerce) {- used _only_ to eliminate fmap traversals for newtype constructors -}

-- * The 'PropertyList' data type
-- (the universal algebra/coalgebra for the unlifted signature)

-- |A fully-parsed property list.
newtype PropertyList = PL { unPL :: FixF PropertyListS }
instance Eq PropertyList where
    PL (InF x) == PL (InF y) = fmap PL x == fmap PL y
instance Ord PropertyList where
    PL (InF x) `compare` PL (InF y) = fmap PL x `compare` fmap PL y

{-# RULES
    -- don't traverse with no-ops!
"fmap PL   -> unsafeCoerce"     fmap PL   = unsafeCoerce
"fmap unPl -> unsafeCoerce"     fmap unPL = unsafeCoerce
  #-}

instance Show PropertyList where
    show pl = showsPrec 0 pl " :: PropertyList"
    showsPrec p (PL x) = showParen (p > 10) $ case outF x of
        PLArray  arr  -> showString "plArray "  . showsPrec 11 (fmap PL arr)
        PLData   bs   -> showString "plData "   . showsPrec 11 bs  
        PLDate   time -> showString "plDate "   . showsPrec 11 time
        PLDict   dict -> showString "plDict "   . showsPrec 11 (fmap PL dict)
        PLReal   dbl  -> showString "plReal "   . showsPrec 11 dbl 
        PLInt    int  -> showString "plInt "    . showsPrec 11 int 
        PLString str  -> showString "plString " . showsPrec 11 str 
        PLBool   bool -> showString "plBool "   . showsPrec 11 bool

instance Copointed f => PListAlgebra f PropertyList where
    {-# SPECIALIZE instance PListAlgebra Identity PropertyList #-}
    plistAlgebra = PL . InF . fmap unPL . extract

instance PListCoalgebra Identity a => PListAlgebra (Either a) PropertyList where
    plistAlgebra = either toPlist (plistAlgebra . Identity)

instance InitialPList Identity PropertyList

instance Pointed f => PListCoalgebra f PropertyList where
    {-# SPECIALIZE instance PListCoalgebra Identity PropertyList #-}
    plistCoalgebra = point . fmap PL . outF . unPL

instance TerminalPList Identity PropertyList

foldPropertyList f (PL pl) = fold pl
    where fold (InF x) = f (fmap fold x)

-- * The 'PartialPropertyList' data type
-- (the universal algebra/coalgebra for the signature extended by 
--  introducing new constructors)

-- |A partially-parsed property-list term algebra, parameterized over the type of
-- \"structural holes\" in the terms.
newtype PartialPropertyList a = PPL {unPPL :: Free PropertyListS a}
    deriving (Pointed, Functor, Monad, MonadFree PropertyListS)
{-# RULES
    -- don't traverse with no-ops!
"fmap PPL   -> unsafeCoerce"     fmap PPL   = unsafeCoerce
"fmap unPPl -> unsafeCoerce"     fmap unPPL = unsafeCoerce
  #-}

instance Applicative PartialPropertyList where
    pure = return
    (<*>) = ap

instance Foldable PartialPropertyList where
    foldMap f (PPL x) = case runFree x of
        Left  x -> f x
        Right x -> foldMap (foldMap f . PPL) x

instance Traversable PartialPropertyList where
    traverse f (PPL x) = case runFree x of
        Left x  -> return <$> f x
        Right x -> inFree <$> traverse (traverse f . PPL) x

instance Eq a => Eq (PartialPropertyList a) where
    PPL x == PPL y = case (runFree x, runFree y) of
        (Left a,  Left  b) -> a == b
        (Right a, Right b) -> fmap PPL a == fmap PPL b
        _                  -> False

instance Ord a => Ord (PartialPropertyList a) where
    PPL x `compare` PPL y = case (runFree x, runFree y) of
        (Left a,  Left  b) -> a `compare` b
        (Left _,  Right _) -> Left () `compare` Right ()
        (Right a, Right b) -> fmap PPL a `compare` fmap PPL b
        (Right _, Left  _) -> Right () `compare` Left ()

instance Show a => Show (PartialPropertyList a) where
    showsPrec p (PPL x) = showParen (p > 10) $ case runFree x of
        Left a ->  showString "return " . showsPrec 11 a
        Right x -> case x of
            PLArray  arr  -> showString "plArray "  . showsPrec 11 (fmap PPL arr)
            PLData   bs   -> showString "plData "   . showsPrec 11 bs  
            PLDate   time -> showString "plDate "   . showsPrec 11 time
            PLDict   dict -> showString "plDict "   . showsPrec 11 (fmap PPL dict)
            PLReal   dbl  -> showString "plReal "   . showsPrec 11 dbl 
            PLInt    int  -> showString "plInt "    . showsPrec 11 int 
            PLString str  -> showString "plString " . showsPrec 11 str 
            PLBool   bool -> showString "plBool "   . showsPrec 11 bool

-- instance Read...


-- this instance overlaps (with incoherence allowed) with all 
-- others for PartialPropertyList: ensure that you don't define 
-- an explicit instance for any 'Copointed' functor!
instance Copointed f => PListAlgebra f (PartialPropertyList a) where
    {-# SPECIALIZE instance PListAlgebra Identity (PartialPropertyList a) #-}
    plistAlgebra = inFree . extract

instance PListAlgebra Maybe (PartialPropertyList ()) where
    plistAlgebra Nothing  = return ()
    plistAlgebra (Just x) = inFree x

instance PListAlgebra (Either a) (PartialPropertyList a) where
    plistAlgebra (Left  x) = return x
    plistAlgebra (Right x) = inFree x

instance InitialPList (Either a) (PartialPropertyList a) where

instance PListCoalgebra (Either a) (PartialPropertyList a) where
    plistCoalgebra (PPL xf) = fmap (fmap PPL) (runFree xf)

instance TerminalPList (Either a) (PartialPropertyList a) where

instance PListCoalgebra Maybe (PartialPropertyList a) where
    plistCoalgebra (PPL xf) = case runFree xf of
        Left  _ -> Nothing
        Right x -> Just (fmap PPL x)

-- * Convenient functions for converting from 'PartialPropertyList' to
-- 'PropertyList'.

-- |Take a 'PartialPropertyList' that has been expunged of all incomplete
-- elements (as witnessed by the 'PListCoalgebra' 'Identity' @a@ context, which
-- states that any value of type @a@ can be unfolded to a complete 'PropertyList')
-- and convert it to a 'PropertyList'.
--
-- This is just a convenient synonym for 'fromPlist' with the types
-- explicitly specialized.
completePropertyList :: PListCoalgebra Identity a => PartialPropertyList a -> PropertyList
completePropertyList = fromPlist

-- |Like 'completePropertyList' but also accepting a function that \"attempts\"
-- to complete any incomplete value in the 'PartialPropertyList'.
-- 
-- Note that there is a potential type-inference trap here - the @b@ parameter
-- needs to be inferrable from the function used.  For example:
-- 
-- > completePropertyListBy (\_ -> fail "parse error")
-- 
-- will be rejected by the compiler because it doesn't know what @b@ is.
--
-- Instead, say:
--
-- > completePropertyListBy (\_ -> fail "parse error" :: IO PropertyList)
-- 
-- (@b@ ~ 'Void' works too, or any other choice of @b@ satisfying the type 
-- context)
completePropertyListBy :: (Applicative f, PListCoalgebra Identity b)
    => (a -> f b) -> PartialPropertyList a -> f PropertyList
completePropertyListBy f = fmap completePropertyList . traverse f

-- |Exactly the same as 'completePropertyListBy', except using 'Monad' in
-- place of 'Applicative' (for situations where a 'Monad' is missing an
-- 'Applicative' instance and you'd rather not add an orphan, etc.)
completePropertyListByM :: (Monad m, PListCoalgebra Identity b)
    => (a -> m b) -> PartialPropertyList a -> m PropertyList
completePropertyListByM f = liftM completePropertyList . unwrapMonad . traverse (WrapMonad . f)

-- instance for Void to allow it to be used as @a@ in 'completePropertyList':
instance Functor f => PListCoalgebra f Void where
    plistCoalgebra = void