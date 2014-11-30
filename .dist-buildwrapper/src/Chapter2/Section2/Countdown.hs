-- Countdown example from chapter 11 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2007.
module Countdown where

  import System.CPUTime
  import Numeric
  import System.IO

-- Expressions
-----------

  data Op                       =  Add | Sub | Mul | Div

  valid                         :: Op -> Int -> Int -> Bool
  valid Add _ _                 =  True
  valid Sub x y                 =  x > y
  valid Mul _ _                 =  True
  valid Div x y                 =  x `mod` y == 0

  apply                         :: Op -> Int -> Int -> Int
  apply Add x y                 =  x + y
  apply Sub x y                 =  x - y
  apply Mul x y                 =  x * y
  apply Div x y                 =  x `div` y
 
  data Expr                     =  Val Int | App Op Expr Expr
 
  values                        :: Expr -> [Int]
  values (Val n)                =  [n]
  values (App _ l r)            =  values l ++ values r
   
  eval                          :: Expr -> [Int]
  eval (Val n)                  =  [n | n > 0]
  eval (App o l r)              =  [apply o x y | x <- eval l
                                                , y <- eval r
                                                , valid o x y]

-- Combinatorial functions
-----------------------

{- Subsets Ordered Lists that comprise the input List
   Test with:
    subs [0]       ==> [[],[0]]
    subs [1,2]     ==> [[],[2],[1],[1,2]]
    subs [1,2,3]   ==> [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
    subs [1,2,3,4] ==> [[],[4],[3],[3,4],[2],[2,4],[2,3],[2,3,4],[1],[1,4],[1,3],[1,3,4],[1,2],[1,2,4],[1,2,3],[1,2,3,4]]
-}

  subs                          :: [a] -> [[a]]
  subs []                       =  [[]]
  subs (x:xs)                   =  yss ++ map (x:) yss
                                   where yss = subs xs
 
  interleave                    :: a -> [a] -> [[a]]
  interleave x []               =  [[x]]
  interleave x (y:ys)           =  (x:y:ys) : map (y:) (interleave x ys)

{- Permutations of a List (different ways the existing List can be ordered, including its original Order)
   Test with:

    perms [1,2]   ==> [[1,2],[2,1]]
    perms [1,2,3] ==> [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}

  perms                         :: [a] -> [[a]]
  perms []                      =  [[]]
  perms (x:xs)                  =  concat (map (interleave x) (perms xs))
 
{- Ex. 0
  Choose a possible definition of the function choices :: [a] -> [[a]], 
  that returns all choices from a list, which are given by all possible 
  ways of selecting zero or more elements in any order.
  Note: the definition of "subs" and "perms" can be found in the given template, 
  make sure that you play around with these functions in GHCi to understand 
  how they work before you attempt to compose them together to implement "choices"
-}
 
{- WRONG Test with:
  choices [] ==> [[]]
  choices [1] ==> [[1],[1,1]]
  choices [1,2] ==> [[1,2],[2,1],[2,1,2],[2,2,1],[1,1,2],[1,2,1],[1,2,1,2],[1,2,2,1]]
     
  choices                       :: [a] -> [[a]]
  choices xs                    = [ys ++ zs | ys <- subs xs, zs <- perms xs]
-}
{- WRONG - incorrect type
  choices                       :: [a] -> [[a]]
  choices xs                    = concat [zs | ys <- subs xs, zs <- perms ys]
-}
{- RIGHT Test with: 
  choices [] ==> [[]]
  choices [1] ==> [[],[1]]
  choices [1,2] ==> [[],[2],[1],[1,2],[2,1]]
-}  
  choices                       :: [a] -> [[a]]
  choices xs                    = [zs | ys <- subs xs, zs <- perms ys]
{--}
{- WRONG Test with:
  choices [] ==> [[]]
  choices [1] ==> [[],[1]]
  choices [1,2] ==> [[],[2],[1],[1,2],[],[1],[2],[2,1]]
  
  choices                       :: [a] -> [[a]]
  choices xs                    = [zs | ys <- perms xs, zs <- subs ys] 
-}
-- Formalising the problem
-----------------------

  solution                      :: Expr -> [Int] -> Int -> Bool
  solution e ns n               =  elem (values e) (choices ns) && eval e == [n]

-- Brute force solution
--------------------

{- Ex. 3
   Choose a correct implementation of the function 
   
   split :: [a] -> [([a],[a])] 
   
   seen in the lecture, that returns all possible ways of 
   splitting a list into two non-empty lists that append to 
   give the original list.
   
   For additional understanding, try to test this function using quickcheck.
-}
{-
  split :: [a] -> [([a], [a])]
  split [] = []
  split [x] = [x]
  split (x : xs) = [([x] : (ls ++ rs)) | (ls, rs) <- split xs]
  
  split :: [a] -> [([a], [a])]
  split [] = []
  split (x : xs) = ([x], xs) : (split xs)
  
  split :: [a] -> [([a], [a])]
  split [] = []
  split (x : xs) = [(x : ls, rs) | (ls, rs) <- split xs]
-}
 
{- RIGHT
  Test with:
  split [1,3,7,10,25,50] ==> 
  [([1],[3,7,10,25,50]),([1,3],[7,10,25,50]),([1,3,7],[10,25,50]),([1,3,7,10],[25,50]),([1,3,7,10,25],[50])]
-}
  
  split :: [a] -> [([a], [a])]
  split [] = []
  split [_] = []
  split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]
  
  
  exprs                         :: [Int] -> [Expr]
  exprs []                      =  []
  exprs [n]                     =  [Val n]
  exprs ns                      =  [e | (ls,rs) <- split ns
                                      , l       <- exprs ls
                                      , r       <- exprs rs
                                      , e       <- combine l r]
  
  combine                       :: Expr -> Expr -> [Expr]
  combine l r                   =  [App o l r | o <- ops]
   
  ops                           :: [Op]
  ops                           =  [Add,Sub,Mul,Div]
  
  solutions                     :: [Int] -> Int -> [Expr]
  solutions ns n                =  [e | ns' <- choices ns
                                      , e   <- exprs ns'
                                      , eval e == [n]]

-- Combining generation and evaluation
-----------------------------------

  type Result                   =  (Expr,Int)
  
  results                       :: [Int] -> [Result]
  results []                    =  []
  results [n]                   =  [(Val n,n) | n > 0]
  results ns                    =  [res | (ls,rs) <- split ns
                                        , lx      <- results ls
                                        , ry      <- results rs
                                        , res     <- combine' lx ry]
  
  combine'                      :: Result -> Result -> [Result]
  combine' (l,x) (r,y)          =  [(App o l r, apply o x y) | o <- ops
                                                             , valid o x y]

  solutions'                    :: [Int] -> Int -> [Expr]
  solutions' ns n               =  [e | ns'   <- choices ns
                                      , (e,m) <- results ns'
                                      , m == n]

-- Exploiting numeric properties
-----------------------------

  valid'                        :: Op -> Int -> Int -> Bool
  valid' Add x y                =  x <= y
  valid' Sub x y                =  x > y
  valid' Mul x y                =  x /= 1 && y /= 1 && x <= y
  valid' Div x y                =  y /= 1 && x `mod` y == 0
  
  results'                      :: [Int] -> [Result]
  results' []                   =  []
  results' [n]                  =  [(Val n,n) | n > 0]
  results' ns                   =  [res | (ls,rs) <- split ns
                                        , lx      <- results' ls
                                        , ry      <- results' rs
                                        , res     <- combine'' lx ry]
  
  combine''                     :: Result -> Result -> [Result]
  combine'' (l,x) (r,y)         =  [(App o l r, apply o x y) | o <- ops
                                                             , valid' o x y]
  
  solutions''                   :: [Int] -> Int -> [Expr]
  solutions'' ns n              =  [e | ns'   <- choices ns
                                      , (e,m) <- results' ns'
                                      , m == n]

-- Interactive version for testing
-------------------------------

  instance Show Op where
     show Add                   =  "+"
     show Sub                   =  "-"
     show Mul                   =  "*"
     show Div                   =  "/"
  
  instance Show Expr where
     show (Val n)               =  show n
     show (App o l r)           =  bracket l ++ show o ++ bracket r
                                   where
                                      bracket (Val n) = show n
                                      bracket e       = "(" ++ show e ++ ")"
  
  showtime                      :: Integer -> String
  showtime t                    =  showFFloat (Just 3)
                                      (fromIntegral t / (10^12)) " seconds"
  
  display                       :: [Expr] -> IO ()
  display es                    =  do t0 <- getCPUTime
                                      if null es then
                                         do t1 <- getCPUTime
                                            putStr "\nThere are no solutions, verified in "
                                            putStr (showtime (t1 - t0))
                                       else
                                         do t1 <- getCPUTime
                                            putStr "\nOne possible solution is "
                                            putStr (show (head es))
                                            putStr ", found in "
                                            putStr (showtime (t1 - t0))
                                            putStr "\n\nPress return to continue searching..."
                                            getLine
                                            putStr "\n"
                                            t2 <- getCPUTime
                                            if null (tail es) then
                                               putStr "There are no more solutions"
                                             else
                                               do sequence [print e | e <- tail es]
                                                  putStr "\nThere were "
                                                  putStr (show (length es))
                                                  putStr " solutions in total, found in "
                                                  t3 <- getCPUTime
                                                  putStr (showtime ((t1 - t0) + (t3 - t2)))
                                      putStr ".\n\n"
  
  main                          :: IO ()
  main                          =  do hSetBuffering stdout NoBuffering
                                      putStrLn "\nCOUNTDOWN NUMBERS GAME SOLVER"
                                      putStrLn "-----------------------------\n"
                                      putStr "Enter the given numbers : "
                                      ns <- readLn
                                      putStr "Enter the target number : "
                                      n  <- readLn
                                      display (solutions'' ns n)
{- Ex. 1
   Choose the correct implementation of a function 
   
   removeone :: Eq a => a -> [a] -> [a]
  
   that removes the first occurence of a given element from a list.
-}
{- WRONG - y not in scope
  removeone :: Eq a => a -> [a] -> [a]
  removeone x [] = [x]
  removeone x ys
    | x == head ys = ys
    | otherwise = y : removeone x ys
-}
{- WRONG 
  Tested with: 
    removeone 2 [1,2,3,4,5]
    [2,1,2,3,4]
  Incorrectly remove last element and added given element at start  
  
  removeone :: Eq a => a -> [a] -> [a]
  removeone x [] = []
  removeone x (y : ys)
    | x == y = ys
    | otherwise = x : removeone y ys
-}
{- WRONG 
  removeone :: Eq a => a -> [a] -> [a]
  removeone x [] = []
  removeone x ys
    | x == head ys = ys
    | otherwise = removeone x ys
-}
{- RIGHT
  Test with: 
    removeone 3 [2,3,5,1,5,3]
    [2,5,1,5,3]
-}    
  removeone :: Eq a => a -> [a] -> [a]
  removeone x [] = []
  removeone x (y : ys)
    | x == y = ys
    | otherwise = y : removeone x ys    


{- Ex. 2
   Choose a correct implementation of the function 
   
   isChoice :: Eq a => [a] -> [a] -> Bool 
   
   that decides whether one list is chosen from another. 
   In other words, "isChoice xs ys" checks whether all elements 
   in "xs" are present in "ys".
-}
{- RIGHT
  isChoice :: Eq a => [a] -> [a] -> Bool 
  
  isChoice [] _ = True
  isChoice (x : xs) [] = False
  isChoice (x : xs) ys = elem x ys && isChoice xs (removeone x ys)
-}
{- WRONG
  isChoice :: Eq a => [a] -> [a] -> Bool 

  isChoice [] _ = False
  isChoice (x : xs) [] = True
  isChoice (x : xs) (y : ys) 
    = elem y xs && isChoice xs (removeone x ys)
-}

{- WRONG

  isChoice :: Eq a => [a] -> [a] -> Bool 

  isChoice [] _ = True
  isChoice xs [] = True
  isChoice xs ys
    = elem (head xs) ys && isChoice xs (removeone (head y) ys)
-}
{- WRONG
  Test with:
  isChoice [1..2] [] ==> False
  isChoice [1, 2, 3] [3, 2, 1] ==> True
  
  isChoice :: Eq a => [a] -> [a] -> Bool 

  isChoice [] _ = True
  isChoice (x : xs) [] = False
  isChoice (x : xs) ys
    = elem x ys && isChoice (removeone x xs) ys
-}

