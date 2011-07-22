{-# LANGUAGE TemplateHaskell #-}
module Data.PropertyList.Binary.Float
    ( doubleToWord64
    , word64ToDouble
    , word32ToDouble

    , floatToWord32
    , word32ToFloat
    ) where

import Foreign
import GHC.Float

-- TODO: create a library or extend an existing on to include a module Data.Float.IEEE
-- which exports types Float32, Float64, etc., with proper IEEE-safe conversions
-- and C union-style conversions to corresponding Word types...

$( do
    let assertFloatProp :: (Monad m, Eq a) => String -> (Float -> a) -> a -> m ()
        assertFloatProp desc p x
            | p (undefined :: Float) == x   = return ()
            | otherwise                     = fail desc
        
        assertDoubleProp :: (Monad m, Eq a) => String -> (Double -> a) -> a -> m ()
        assertDoubleProp desc p x
            | p (undefined :: Double) == x  = return ()
            | otherwise                     = fail desc
    
    assertFloatProp "Float should ahdere to the IEEE-754 standard" 
        isIEEE True
    assertFloatProp "Float's size should be 32 bits"
        sizeOf 4
    assertFloatProp "Float should have a base-2 mantissa"
        floatRadix 2
    assertFloatProp "Float should have a 23-bit mantissa"
        floatDigits 24
    assertFloatProp "Float should have an 8-bit exponent"
        floatRange (-125, 128)
    
    assertDoubleProp "Double should ahdere to the IEEE-754 standard" 
        isIEEE True
    assertDoubleProp "Double's size should be 64 bits"
        sizeOf 8
    assertDoubleProp "Double should have a base-2 mantissa"
        floatRadix 2
    assertDoubleProp "Double should have a 52-bit mantissa"
        floatDigits 53
    assertDoubleProp "Double should have an 11-bit exponent"
        floatRange (-1021, 1024)
    
    return []
 )
 
doubleToWord64 :: Double -> Word64
doubleToWord64 = unsafeConvertStorable

word64ToDouble :: Word64 -> Double
word64ToDouble = unsafeConvertStorable

word32ToDouble :: Word32 -> Double
word32ToDouble = float2Double . word32ToFloat

floatToWord32 :: Float -> Word32
floatToWord32 = unsafeConvertStorable

word32ToFloat :: Word32 -> Float
word32ToFloat = unsafeConvertStorable

unsafeConvertStorable :: (Storable a, Storable b) => a -> b
unsafeConvertStorable x = unsafePerformIO $ 
    alloca $ \p -> do
        poke p x
        peek (castPtr p)

