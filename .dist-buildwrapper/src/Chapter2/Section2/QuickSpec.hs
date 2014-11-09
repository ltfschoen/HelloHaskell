{-# LANGUAGE TemplateHaskell #-}
-- check properties using QuickCheck
module QuickSpec where

  import BooleanFormula (eval)
  import Test.QuickCheck

  -- two properties toDigits that should hold for any vaid input n >= 0

  -- eval (toDigits n) == n
  -- all (\d -> d >= 0 && d < 10) (toDigits n)

  prop_toDigits n = n >= 0 ==> eval (toDigits n) == n
  prop_toDigits2 n = n >= 0 ==> all (\d -> d >= 0 && d < 10) (toDigits n)
  eval xs = foldl (\x y -> y + x*10) 0 xs

  -- test in GHCi and should return 'OK, passed 100 tests'
  -- quickCheck prop_toDigits
  -- quickCheck prop_toDigits2