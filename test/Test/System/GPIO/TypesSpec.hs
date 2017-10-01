{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module Test.System.GPIO.TypesSpec (spec) where

import Protolude
import System.GPIO.Types

import Data.Bits (unsafeShiftL, unsafeShiftR)
import Test.Hspec
import Test.QuickCheck (property)

-- Test all our hand-derived instances and functions.

-- PinValue mimics Bool with respect to Bits and FiniteBits.
--
ib2v :: (Int -> Bool) -> Int -> PinValue
ib2v f n = boolToValue $ f n

bi2v :: (Bool -> Int) -> PinValue -> Int
bi2v f a = f (valueToBool a)

bb2v :: (Bool -> Bool) -> PinValue -> PinValue
bb2v f a = boolToValue $ f (valueToBool a)

bbb2v :: (Bool -> Bool -> Bool) -> PinValue -> PinValue -> PinValue
bbb2v f a b = boolToValue $ f (valueToBool a) (valueToBool b)

bib2v :: (Bool -> Int -> Bool) -> PinValue -> Int -> PinValue
bib2v f a n = boolToValue $ f (valueToBool a) n

#if MIN_VERSION_base(4,8,0)
newBase :: Spec
newBase =
  do context "implements the new base-4.8.0.0 FiniteBits typeclass methods" $
       do it "countLeadingZeros" $ property $
            \a -> countLeadingZeros a == bi2v countLeadingZeros a
          it "countTrailingZeros" $ property $
            \a -> countTrailingZeros a == bi2v countTrailingZeros a
#else
newBase :: Spec
newBase = return ()
#endif

spec :: Spec
spec =
  do describe "Pin" $
       do it "pinNumber" $ property $
            \p@(Pin n) -> n == pinNumber p

     describe "PinDirection" $
       do it "invertDirection" $
            invertDirection In == Out
              && invertDirection Out == In

     describe "PinValue" $
       do it "invertValue" $ property $
            \a -> invertValue a == complement a

          it "valueToBool" $
            valueToBool Low == False
              && valueToBool High == True

          it "boolToValue" $
            boolToValue False == Low
              && boolToValue True == High

          context "implements the Bits typeclass" $
           do it "(.&.)" $ property $
                \a b -> a .&. b == bbb2v (.&.) a b

              it "(.|.)" $ property $
                \a b -> a .|. b == bbb2v (.|.) a b

              it "xor" $ property $
                \a b -> a `xor` b == bbb2v xor a b

              it "complement" $ property $
                \a -> complement a == bb2v complement a

              it "shift" $ property $
                \a n -> a `shift` n == bib2v shift a n

              it "rotate" $ property $
                \a n -> a `rotate` n == bib2v rotate a n

              it "zeroBits" $ property $
                (zeroBits :: PinValue) == boolToValue (zeroBits:: Bool)

              it "bit" $ property $
                \n -> bit n == ib2v bit n

              it "setBit" $ property $
                \a n -> a `setBit` n == bib2v setBit a n

              it "clearBit" $ property $
                \a n -> a `clearBit` n == bib2v clearBit a n

              it "complementBit" $ property $
                \a n -> a `complementBit` n == bib2v complementBit a n

              it "testBit" $ property $
                \a n -> testBit a n == testBit (valueToBool a) n

              it "bitSizeMaybe" $ property $
                \a -> bitSizeMaybe a == bitSizeMaybe (valueToBool a)

              it "bitSize" $ property $
                \a -> bitSize a == bitSize (valueToBool a)

              it "isSigned" $ property $
                \a -> isSigned a == isSigned (valueToBool a)

              it "shiftL" $ property $
                \a n -> a `shiftL` n == bib2v shiftL a n

              it "unsafeShiftL" $ property $
                \a n -> a `unsafeShiftL` n == bib2v unsafeShiftL a n

              it "shiftR" $ property $
                \a n -> a `shiftR` n == bib2v shiftR a n

              it "unsafeShiftR" $ property $
                \a n -> a `unsafeShiftR` n == bib2v unsafeShiftR a n

              it "rotateL" $ property $
                \a n -> a `rotateL` n == bib2v rotateL a n

              it "rotateR" $ property $
                \a n -> a `rotateR` n == bib2v rotateR a n

              it "popCount" $ property $
                \a -> popCount a == popCount (valueToBool a)

          context "implements the FiniteBits typeclass" $
            do it "finiteBitSize" $ property $
                 \a -> finiteBitSize a == bi2v finiteBitSize a

               newBase
