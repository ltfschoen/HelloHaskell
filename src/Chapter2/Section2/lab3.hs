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

-- complete the following line with the correct type signature for this function
-- squares :: ... 
  squares n = undefined

  sumSquares :: Integer -> Integer
  sumSquares n = sum (squares n)

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
-- squares' :: ...
  squares' m n = undefined

  sumSquares' :: Integer -> Integer
  sumSquares' x = sum . uncurry squares' $ (x, x)

-- ===================================
-- Ex. 8
-- ===================================

  coords :: Integer -> Integer -> [(Integer,Integer)]
  coords = undefined
