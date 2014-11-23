-- flag to turn on n+k patterns in ghc
{-# LANGUAGE NPlusKPatterns #-}
module Chapter2.Section2.Lab4 where
  import Prelude

------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

--import Data.Char

-- ===================================
-- Ex. 0
-- ===================================

  -- RIGHT (wrong approach though)
  triangle :: Integer -> Integer
  triangle n = foldr (+) 0 [1..n]

-- ===================================
-- Ex. 1
-- ===================================

{-
   Define a recursive function that counts how many times a given value
   occurs in a list using the following shape (rather than with Predule 
   functions or list comprehensions

   count a [] = ...
   count a (x:xs) = ... a ... x ... count a xs ...
   Example: count "Haskell" ["Java", "PHP", "Javascript", "C#"] = 0
   Example: count 'e' "The quick brown fox jumped over the lazy dog." = 4

   Given the lists xs and ys that are defined in the template for this lab, 
   what is the result of: count 722 ys?
-}

  count :: Eq a => a -> [a] -> Int
  count a (x:xs) = eval a >>= (\ x -> count a xs)

  {-
xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]
  -}
-- ===================================
-- Ex. 2
-- ===================================

  --euclid :: (Int,  Int) -> Int
  --euclid (x, y) = undefined

-- ===================================
-- Ex. 3
-- ===================================

  --funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
  --funkyMap f g xs = undefined