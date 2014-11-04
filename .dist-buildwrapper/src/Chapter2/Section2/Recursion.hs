{-# LANGUAGE NoImplicitPrelude #-}

module Chapter2.Section2.Recursion where
  import Data.Char
  import Prelude hiding (
    and, concat, replicate, (!!), elem)
  -- import Data.List hiding ( and )
    
-- http://stackoverflow.com/questions/16430025/ambiguous-occurrence
-- overcome error Ambiguous occurrence ‘^’
-- import Prelude hiding (Ord, compare, (<), (<=), (>=), (>), max, min)

  -- (^) :: Int -> Int -> Int
  -- WRONG
  -- m ^ 0 = 0
  -- m ^ n = m * m ^ (n - 1)
  -- RIGHT
  -- m ^ 0 = 1
  -- m ^ n = m * m ^ (n - 1)
  -- WRONG (crashes on 0^1)
  -- m ^ 0 = 1
  -- m ^ n = m * m ^ n - 1  
  -- WRONG  (0^1=-1 is incorrect)
  -- m ^ 0 = 1
  -- m ^ n = n * n ^ (m - 1) 
  -- RIGHT
  -- m ^ 0 = 1
  -- m ^ n = m * (^) m (n - 1) 
  -- WRONG - CRASHES
  -- m ^ 0 = 1
  -- m ^ n = m * m * m ^ (n - 2)
  -- WRONG - 0 ^ 1 is not 1
  -- m ^ 0 = 1
  -- m ^ n = (m * m) ^ (n - 1)
  -- WRONG - 1 ^ 0 CRASHES
  -- m ^ 1 = m
  -- m ^ n = m * m ^ (n - 1)  
  
  {- 
    Test with:
      and [True, True, True] -- True
      and [True, False, True] -- False
      -- and other False value positions in the AND Truth Table
  -}
  
  {- RIGHT
  and :: [Bool] -> Bool
  and [] = True
  and (b : bs) = b && and bs
  -}
  {- RIGHT
  and [] = True
  and (b : bs)
    | b = and bs
    | otherwise = False
  -}
  {- WRONG where False in list it outputs True
  and [] = True
  and (b : bs)
    | b = b
    | otherwise = and bs
  -}
  {- WRONG as where False in list it outputs True
  and [] = True
  and (b : bs) = b || and bs
  -}
  {- RIGHT
  and [] = True
  and (b : bs)
    | b == False = False
    | otherwise = and bs
  -}
  {- RIGHT
  and [] = True
  and (b : bs) = and bs && b
  -}
  
  -- Choose the correct definition for the function that concatenates a list of lists:
  -- Test with:
  --   concat [[1..10], [2,4], [20..25]]
  -- Expected output:
  --   [1,2,3,4,5,6,7,8,9,10,2,4,20,21,22,23,24,25]
  {- WRONG due to Type issue
  concat :: [[a]] -> [a]
  concat [] = []
  concat (xs : xss) = xs : concat xss
  -}
  {- RIGHT
  concat :: [[a]] -> [a]
  concat [] = []
  concat (xs : xss) = xs ++ concat xss
  -}
  {- WRONG as breaches Type signature
  concat :: [[a]] -> [a]
  concat [] = [[]]
  concat (xs : xss) = xs ++ concat xss
  -}
  {- WRONG as does not match for the pattern [] so is non-exhaustive
  concat :: [[a]] -> [a]
  concat [[]] = []
  concat (xs : xss) = xs ++ concat xss
  -}
  
  -- Choose the correct definition for the function that produces a list with n identical elements:
  -- Test with:
  --   replicate 3 5
  -- Expected output: [5,5,5]
  -- Test with:
  --   replicate 10 'a'
  -- Expected output: "aaaaaaaaaa"
  {- WRONG as type signature expects a list Type
  replicate :: Int -> a -> [a]
  replicate 1 x = x
  replicate n x = x : replicate (n - 1) x
  -}
  {- WRONG due to infinite Type issue
  replicate 0 _ = []
  replicate n x = replicate (n - 1) x : x
  -}
  {- WRONG as does not replicate output the correct amount of times
  replicate 1 _ = []
  replicate n x = replicate (n - 1) x ++ [x]
  -}
  {- RIGHT
  replicate 0 _ = []
  replicate n x = x : replicate (n - 1) x
  -}
  
  -- Choose the correct definition for the function that 
  -- selects the n th element of a list. We start counting at 0.
  -- Test with:
  --   [1,2,3] !! 2
  -- Expected output: 3
  -- Test with:
  --   [1,2,3] !! 0
  -- Expected output: 1
  {- WRONG as does not start at 0th element
  (!!) :: [a] -> Int -> a
  (x : _) !! 1 = x
  (_ : xs) !! n = xs !! (n - 1)
  -}
  {- WRONG as last line fails
  (!!) :: [a] -> Int -> a
  (x : _) !! 0 = x
  (_ : xs) !! n = xs !! (n - 1)
  [] !! n = 0
  -}
  {- RIGHT
  (x : _) !! 0 = x
  (_ : xs) !! n = xs !! (n - 1)
  -}
  {- WRONG
  (x : _) !! 0 = [x]
  (_ : xs) !! n = xs !! (n - 1)
  -}
  
  -- Choose the correct definition for the function that decides
  -- if a value is an element of a list:
  -- Test with:
  --   elem 1 [10,5,4]
  -- Expected output: False
  -- Test with
  --   elem 'a' ['b', 'a']
  -- Expected output: True
  {- RIGHT
  elem :: Eq a => a -> [a] -> Bool
  elem _ [] = False
  elem x (y : ys)
    | x == y = True
    | otherwise = elem x ys
  -}
  
  {- 
    Choose the correct definition for the function 
    merge :: Ord a => [a] -> [a] -> [a] that merges two
    sorted lists in ascending order to give a single 
    sorted list in ascending order.
    Example: 
      > merge [2, 5, 6] [1, 3, 4]
      [1, 2, 3, 4, 5, 6]  
  -}
  {- WRONG
  merge :: Ord a => [a] -> [a] -> [a]
  merge [] ys = ys
  merge xs [] = xs
  merge (x : xs) (y : ys)
    = if x <= y then x : merge xs ys else y : merge xs ys 
  -}
  {- WRONG jumbled result
  merge :: Ord a => [a] -> [a] -> [a]
  merge [] ys = ys
  merge xs [] = xs
  merge (x : xs) (y : ys)
    = if x <= y then y : merge xs (y : ys) else x : merge (x : xs) ys
  -}
  {- WRONG, merges correctly, but does not order them
  merge :: Ord a => [a] -> [a] -> [a]
  merge [] ys = ys
  merge xs [] = xs
  merge (x : xs) (y : ys)
    = if x <= y then y : merge (x : xs) ys else x : merge xs (y : ys)
  -}
  {- RIGHT -}
  merge :: Ord a => [a] -> [a] -> [a]
  merge [] ys = ys
  merge xs [] = xs
  merge (x : xs) (y : ys)
    = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys
  {- -}
  {-
    Choose the correct definition for the function msort :: Ord a => [a] -> [a] that
    implements merge sort, in which the empty list and singleton lists are already sorted, 
    and any other list is sorted by merging together the two lists that result from sorting
    the two halves of the list separately. The solutions can use the function merge from the
    previous exercise and the function halve that splits a list into two halves whose lengths
    differ by at most one.
    Test with: halve [1,2,3]
    Expected output: ([1],[2,3])
    Test with: fst ([1],[2,3])
    Expected output: [1]
    Test with: snd ([1],[2,3])
    Expected output: [2,3]
  -}
  halve :: [a] -> ([a], [a])
  halve xs = splitAt (length xs `div` 2) xs
  msort :: Ord a => [a] -> [a]
  
  {- WRONG freezes on msort [3,2,90,54,1]
  msort [] = []
  msort xs = merge (msort zs) (msort ys)
    where (ys, zs) = halve xs
  -}
  {- RIGHT
  msort [] = []
  msort [x] = [x]
  msort xs = merge (msort zs) (msort ys)
    where (ys, zs) = halve xs
 -}
 {- WRONG - crashes on same  input
  msort [] = []
  msort [x] = [x]
  msort xs = msort xs ++ msort zs
    where (ys, zs) = halve xs
 -}
 {- WRONG - crashes on same  input
  msort [] = []
  msort [x] = [x]
  msort xs = (msort xs ++ msort zs)
    where (ys, zs) = halve xs
 -}