module Lab3 where
  import Prelude
-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

  {-
    Using a list comprehension, define a function that selects all the even numbers from a list.
    Example: evens [2, 5, 6, 13, 32] = [2, 6, 32]
    Test with: evens [827305 .. 927104]
    Then copy the outcome and paste it after 'sumevens'
    Gives answer: 43772529500
    
    Note:
    sum . evens $ [827305 .. 927104] is equivalent to:
    (sum . evens) [827305 .. 927104]
    whereas
    sum . evens [827305 .. 927104] is equivalent to:
    sum . (evens [827305 .. 927104])
    Now the types don't line up.
    Another (correct) option would be to write it as follows:
    sum (evens [827305 .. 927104])
  -}
  evens :: [Integer] -> [Integer]
  -- WRONG evens n = [x | x <- [1..10], n `mod` 2 == 0]
  evens ints = filter even (ints)
  
  {- Return sum of even integers from list. See page 63 and 71 text -}
  sumevens :: [Integer] -> Integer
  sumevens ns = sum(filter even ns)
-- ===================================
-- Ex. 3 - 4 
-- ===================================

  {-
    Using a list comprehension, define a function squares that
    takes a non-bottom Integer n >= 0 as its argument and returns
    a list of the numbers [1..n] squared.
    
    Example:
      squares 4 = [1*1, 2*2, 3*3, 4*4]
      squares 0 = []
  -}

-- complete the following line with the correct type signature for this function
  {- Squares each element of an array -}
  {-
  squares :: [Int] -> [Int]
  squares (x:xs) = x * x : squares xs
  squares [] = []
  -}

  -- WRONG squares :: Num -> [Num]
  -- WRONG squares :: Integer a => a -> [a]
  -- WRONG squares :: a -> [a]
  -- RIGHT
  squares :: Integer -> [Integer]
  squares n = [n^2 | n <- [1..n]]

  -- Example:
  -- *Lab3> sumSquares 50
  -- Result:
  -- 42925
  sumSquares :: Integer -> Integer
  sumSquares n = sum (squares n)

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
{-
  Modify the previous definition of squares such that it now takes two non-bottom 
  Integer arguments, m >= 0 and n >= 0 and returns a list of the m square numbers 
  that come after the first n square numbers.

  Example: 
    squares' 4 2 = [3*3, 4*4, 5*5, 6*6] 
    squares' 2 0 = [1*1, 2*2]
    squares' 0 2 = []
    squares' 0 0 = []
-}
  squares' :: Integer -> Integer -> [Integer]
  squares' m n = [m^2 | m <- [(n+1)..(m+n)]]

  
  {- 
    Example:
    sum $ squares' 10 0
    -- is equivalent to:
    *Lab3> sum (squares' 10 0)
    -- Returns:
    385
  -}
  {- 
    Example:
    sum $ squares' 0 10
    -- is equivalent to:
    *Lab3> sum (squares' 0 10)
    -- Returns:
    0
  -}
  sumSquares' :: Integer -> Integer
  sumSquares' x = sum . uncurry squares' $ (x, x)

-- ===================================
-- Ex. 8
-- ===================================


  {- 
    Using a list comprehension, define a function 
    coords :: Integer -> Integer -> [(Integer, Integer)] 
    that returns a list of all coordinate pairs on an 
    [0..m] × [0..n] rectangular grid, where m and n are non-bottom Integers >= 0.
    Example:
      coords 1 1 = [(0,0), (0,1), (1,0), (1,1)]
      coords 1 2 = [(0,0), (0,1), (0,2), (1,0), (1, 1), (1, 2)]
    Test: What is the value of: foldr (-) 0 . map (uncurry (*)) $ coords 5 7
    *Lab3> (foldr (-) 0 . map (uncurry (*))) (coords 5 7)
    Answer: -60
  -}
  -- referencing the pyths example in Chapter 5
  coords :: Integer -> Integer -> [(Integer,Integer)]
  coords m n = [(x,y) | x <- [0..m],
                        y <- [0..n]]
