{-# LANGUAGE 
    MultiParamTypeClasses,
    FlexibleContexts, FlexibleInstances, IncoherentInstances,
    GeneralizedNewtypeDeriving
  #-}

-- |This module implements the 'PropertyList' and 'PartialPropertyList' types
-- and their algebras/coalgebras.  These types are the core of the property-list
-- implementation, representing either complete or partial propertylists, 
-- respectively, in the most \"universal\" form possible.
module Data.PropertyList.Types
    ( -- * The 'PropertyList' data type
      -- (the universal algebra/coalgebra for the unlifted signature)
      
      PropertyList
      
      -- * The 'PartialPropertyList' data type
      -- (the universal algebra/coalgebra for the signature extended by 
      --  introducing new constructors)
    , PartialPropertyList
      
      -- * Convenient functions for converting from 'PartialPropertyList' to
      -- 'PropertyList'.
    , completePropertyList
    , completePropertyListBy
    , completePropertyListByM
    ) where

import Data.PropertyList.Algebra

import Control.Applicative      (Applicative(..))
import Data.Functor.Foldable    (Fix(..))
import Data.Pointed             (Pointed(..))
import Data.Copointed           (Copointed(..))
import Control.Monad            (liftM)
import Control.Monad.Free       (Free(..))
import Control.Monad.Identity   (Identity(..))
import Data.Foldable            (Foldable)
import Data.Traversable         (Traversable(traverse), mapM)
import Data.Void                (Void, absurd)

import Unsafe.Coerce            (unsafeCoerce) {- used _only_ to eliminate fmap traversals for newtype constructors -}

-- |A fully-parsed property list.
newtype PropertyList = PL { unPL :: Fix PropertyListS }
    deriving (Eq, Ord)

{-# RULES
    -- don't traverse with no-ops!
"fmap PL   -> unsafeCoerce"     fmap PL   = unsafeCoerce
"fmap unPl -> unsafeCoerce"     fmap unPL = unsafeCoerce
  #-}

instance Show PropertyList where
    show pl = showsPrec 0 pl " :: PropertyList"
    showsPrec p (PL (Fix x)) = showParen (p > 10) $ case x of
        PLArray  arr  -> showString "plArray "  . showsPrec 11 (fmap PL arr)
        PLData   bs   -> showString "plData "   . showsPrec 11 bs  
        PLDate   time -> showString "plDate "   . showsPrec 11 time
        PLDict   dict -> showString "plDict "   . showsPrec 11 (fmap PL dict)
        PLReal   dbl  -> showString "plReal "   . showsPrec 11 dbl 
        PLInt    int  -> showString "plInt "    . showsPrec 11 int 
        PLString str  -> showString "plString " . showsPrec 11 str 
        PLBool   bool -> showString "plBool "   . showsPrec 11 bool

instance (Functor f, Copointed f) => PListAlgebra f PropertyList where
    {-# SPECIALIZE instance PListAlgebra Identity PropertyList #-}
    plistAlgebra = PL . Fix . fmap unPL . copoint

instance PListCoalgebra Identity a => PListAlgebra (Either a) PropertyList where
    plistAlgebra = either toPlist (plistAlgebra . Identity)

instance InitialPList Identity PropertyList

instance (Functor f, Pointed f) => PListCoalgebra f PropertyList where
    {-# SPECIALIZE instance PListCoalgebra Identity PropertyList #-}
    plistCoalgebra = point . fmap PL . outF . unPL
        where outF (Fix x) = x

instance TerminalPList Identity PropertyList

-- |A partially-parsed property-list term algebra, parameterized over the type of
-- \"structural holes\" in the terms.
newtype PartialPropertyList a = PPL {unPPL :: Free PropertyListS a}
    deriving (Eq, Ord, Functor, Applicative, Monad, Foldable, Traversable)
{-# RULES
    -- don't traverse with no-ops!
"fmap PPL   -> unsafeCoerce"     fmap PPL   = unsafeCoerce
"fmap unPPl -> unsafeCoerce"     fmap unPPL = unsafeCoerce
  #-}


instance Pointed PartialPropertyList where
    point = PPL . Pure

-- | [internal] 'Free' constructor specialized to 'PartialPropertyList'.
-- 'point'/'pure'/'return' is the corresponding 'Pure' constructor.
inPPL :: PropertyListS (PartialPropertyList a) -> PartialPropertyList a
inPPL = PPL . Free . fmap unPPL

instance Show a => Show (PartialPropertyList a) where
    showsPrec p (PPL x) = showParen (p > 10) $ case x of
        Pure a ->  showString "return " . showsPrec 11 a
        Free x -> case x of
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
instance (Functor f, Copointed f) => PListAlgebra f (PartialPropertyList a) where
    {-# SPECIALIZE instance PListAlgebra Identity (PartialPropertyList a) #-}
    plistAlgebra = inPPL . copoint

instance PListAlgebra Maybe (PartialPropertyList ()) where
    plistAlgebra Nothing  = point ()
    plistAlgebra (Just x) = inPPL x

instance PListAlgebra (Either a) (PartialPropertyList a) where
    plistAlgebra (Left  x) = point x
    plistAlgebra (Right x) = inPPL x

instance InitialPList (Either a) (PartialPropertyList a) where

instance PListCoalgebra (Either a) (PartialPropertyList a) where
    plistCoalgebra (PPL (Pure a)) = Left  a
    plistCoalgebra (PPL (Free a)) = Right (fmap PPL a)

instance TerminalPList (Either a) (PartialPropertyList a) where

instance PListCoalgebra Maybe (PartialPropertyList a) where
    plistCoalgebra (PPL (Pure _)) = Nothing
    plistCoalgebra (PPL (Free x)) = Just (fmap PPL x)

-- |Take a 'PartialPropertyList' that has been expunged of all incomplete
-- elements (as witnessed by the 'PListCoalgebra' 'Identity' @a@ context, which
-- states that any value of type @a@ can be unfolded to a complete 'PropertyList')
-- and convert it to a 'PropertyList'.
--
-- This is just a convenient synonym for 'fromPlist' with the types
-- explicitly specialized.
completePropertyList :: PListCoalgebra Identity a => PartialPropertyList a -> PropertyList
completePropertyList = foldPList
    (plistAlgebra :: PListCoalgebra Identity a => Either a (PropertyListS PropertyList) -> PropertyList)

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
completePropertyListByM f = liftM completePropertyList . Data.Traversable.mapM f

-- instance for Void to allow it to be used as @a@ in 'completePropertyList':
instance Functor f => PListCoalgebra f Void where
    plistCoalgebra = absurd