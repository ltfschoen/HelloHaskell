module HighOrder where
  import Prelude hiding (
    all, dropWhile, takeWhile, any, mapM, fmap, max, curry, uncurry, iterate)
  -- DON'T UNDERSTAND!!!
  -- Test with 'all even [1,2,3,4]' -- expect return False
  -- Test with 'all odd [1,3,7]' -- expect return True
  -- all :: (a -> Bool) -> [a] -> Bool
  -- RIGHT all p xs = and (map p xs)
  -- RIGHT all p xs = map p (and xs)
  -- RIGHT all p = and . map p
  -- RIGHT all p = not . any (not . p)
  -- WRONG all p = map p . and
  -- RIGHT all p xs = foldl (&&) True (map p xs)
  -- WRONG all p xs = foldr (&&) False (map p xs) -- returns 
  -- RIGHT all p = foldr (&&) True . map p
  
  -- terminal message: Top-level binding with no type signature:
  -- dropWhile :: forall t. (t -> Bool) -> [t] -> [t]
  -- test with dropWhile odd [1..10]
  -- returns: [2,3,4,5,6,7,8,9,10]
  {- RIGHT
  dropWhile _ [] = []
  dropWhile p (x : xs)
    | p x = dropWhile p xs
    | otherwise = x : xs
  -}
  
  -- Top-level binding with no type signature:
  -- dropWhile :: forall t. (t -> Bool) -> [t] -> [t]
  -- test with dropWhile odd [1..10]
  -- returns: [3,4,5,6,7,8,9,10]
  {- WRONG
  dropWhile _ [] = []
  dropWhile p (x : xs)
    | p x = dropWhile p xs
    | otherwise = xs
  -}
  
  -- Top-level binding with no type signature:
  -- dropWhile :: forall a. (a -> Bool) -> [a] -> [a]
  -- test with dropWhile odd [1..10]
  -- returns: [2,4,6,8,10]
  -- test with dropWhile (<2) [0,1]
  -- returns: []
  {- WRONG performs more like \p -> filter (not . p)
  dropWhile p = foldr (\ x acc -> if p x then acc else x : acc) []  
  -}
  
  -- Top-level binding with no type signature:
  -- dropWhile :: forall a. (a -> Bool) -> [a] -> [a]
  -- test with dropWhile odd [1..10]
  -- returns: [10,9,8,7,6,5,4,3,2]
  -- test with dropWhile (<2) [0,1]
  -- returns: []
  {- WRONG
  dropWhile p = foldl add []
    where add [] x = if p x then [] else [x]
          add acc x = x : acc
  -}
  
  -- test with: takeWhile odd [1,2,3,4,5,6,7,8,9,10]
  -- returns: [1]
  -- test with: takeWhile even [1,2,3,4,5,6,7,8,9,10]
  -- returns: [] 
  -- test with: takeWhile odd []
  -- returns: [] 
  -- takeWhile :: (a -> Bool) -> [a] -> [a]
  {- WRONG
  takeWhile _ [] = []
  takeWhile p (x : xs)
    | p x = x : takeWhile p xs
    | otherwise = takeWhile p xs
  -}
  -- takeWhile even [1,2,3,4,5,6,7,8,9,10]
  -- returns: []
  -- takeWhile odd [1,2,3,4,5,6,7,8,9,10]
  -- returns: []
  {- RIGHT
  takeWhile _ [] = []
  takeWhile p (x : xs)
    | p x = x : takeWhile p xs
    | otherwise = []
  -}
  
  -- takeWhile even [1,2,3,4,5,6,7,8,9,10]
  -- returns: []
  {- WRONG
  takeWhile _ [] = []
  takeWhile p (x : xs)
    | p x = takeWhile p xs
    | otherwise = []  
  -}
  -- takeWhile even [1,2,3,4,5,6,7,8,9,10]
  -- returns: [10,8,6,4,2]
  {- WRONG
  takeWhile p = foldl (\ acc x -> if p x then x : acc else acc) []
  -}
  
  -- test with: any even [1,2,3]
  -- test with: any even [1,3]
  -- test with: any odd []
  -- any :: (a -> Bool) -> [a] -> Bool
  
  -- WRONG any p = map p . or
  -- RIGHT any p = or . map p 
  -- RIGHT any p xs = length (filter p xs) > 0 
  -- RIGHT
  {- 
  dropWhile _ [] = []
  dropWhile p (x : xs)
    | p x = dropWhile p xs
    | otherwise = x : xs
  any p = not . null . dropWhile (not . p)
  -}
  -- WRONG any p = null . filter p
  -- RIGHT
  {-
  all p = and . map p
  any p xs = not (all (\ x -> not (p x)) xs)
  -}
  -- RIGHT any p xs = foldr (\ x acc -> (p x) || acc) False xs
  -- WRONG any p xs = foldr (||) True (map p xs)
  
  {- 
  Choose the option that implements the Prelude function
  taking into account only finite, non-partial input lists 
  with non-bottom values and where the mapping function does
  not return bottom.
  -}
  -- test with: map odd [0,1,2,3,4,5]
  -- test with: map odd []
  -- map :: (a -> b) -> [a] -> [b]
  
  -- returns: [True,False,True,False,True,False]
  -- returns: []
  -- WRONG map f = foldr (\ x xs -> xs ++ [f x]) []
  -- crashes
  -- WRONG map f = foldr (\ x xs -> f x ++ xs) []
  -- returns: [True,False,True,False,True,False]
  -- returns: []
  -- WRONG map f = foldl (\ xs x -> f x : xs) []
  -- returns: [False,True,False,True,False,True]
  -- returns: []
  -- RIGHT map f = foldl (\ xs x -> xs ++ [f x]) []
  
  {- 
  Choose the option that implements the Prelude function
  taking into account only finite, non-partial input lists 
  with non-bottom values and where the mapping function does
  not return bottom.
  -}
  -- test with: filter odd [0,1,2,3,4,5]
  -- test with: filter odd []
  -- filter :: (a -> Bool) -> [a] -> [a]
  -- returns: [5,3,1]
  -- returns: []
  -- WRONG filter p = foldl (\ xs x -> if p x then x : xs else xs) []
  -- returns: [1,3,5]
  -- returns: []
  -- RIGHT filter p = foldr (\ x xs -> if p x then x : xs else xs) []
  -- returns: [5,3,1]
  -- returns: []
  -- WRONG filter p = foldr (\ x xs -> if p x then xs ++ [x] else xs) []
  -- WRONG filter p = foldl (\ x xs -> if p x then xs ++ [x] else xs) []
  
  {-
    Choose a definition for the function 
    dec2int :: [Integer] -> Integer that converts a finite, 
    non-partial list of non-bottom Integer digits, that represents
    a decimal number, into the non-bottom Integer this list represents. 
    For example:
    > dec2int [2, 3, 4, 5]
    2345
    > dec2int []
    0
    > dec2int [0, 0, 0, 0]
    0
  -}
  dec2int :: [Integer] -> Integer
  
  -- returns: 140
  -- returns: 0
  -- returns: 0
  -- WRONG dec2int = foldr (\ x y -> 10 * x + y) 0

  -- returns: 140
  -- returns: 0
  -- returns: 0
  -- WRONG dec2int = foldl (\ x y -> x + 10 * y) 0

  -- returns: 2345
  -- returns: 0
  -- returns: 0
  -- RIGHT dec2int = foldl (\ x y -> 10 * x + y) 0

  -- returns: 5432
  -- returns: 0
  -- returns: 0
  dec2int = foldr (\ x y -> x + 10 * y) 0
  
  {- 
    Choose explanation why the definition below of sumsqreven is invalid
  -}
  {-
  sumsqreven = compose [sum, map (^ 2), filter even]
  
  compose :: [a -> a] -> (a -> a)
  compose = foldr (.) id
  -}
  -- The definition of sumsqreven doesn't even typecheck
  
  -- curry :: ((a, b) -> c) -> a -> b -> c
  
  -- WRONG curry f = \ x y -> f x y
  -- WRONG curry f = \ x y -> f
  -- RIGHT curry f = \ x y -> f (x, y)
  -- WRONG curry f = \ (x, y) -> f x y
  
  -- uncurry :: (a -> b -> c) -> (a, b) -> c
  
  -- RIGHT uncurry f = \ (x, y) -> f x y
  -- WRONG uncurry f = \ x y -> f (x, y)
  -- WRONG uncurry f = \ (x, y) -> f 
  -- WRONG uncurry f = \ x y -> f 

  {-
  int2list :: Int -> [Int]
  int2list 0 = []
  int2list n = n `mod` 10 : int2list (n `div` 10)
  -}
  
  {-
  -- test with: int2bin 0 --> []
  -- test with: int2bin (-0) --> []  
  type Bit = Int
  int2bin :: Int -> [Bit]
  int2bin 0 = []
  int2bin n = n `mod` 2 : int2bin (n `div` 2)
  -}
  
  unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
  unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t (t x)
  
  -- int2bin = unfold (==0) (`mod` 2) (`div` 2)
  
  {-
  type Bit = Int
  chop8 :: [Bit] -> [[Bit]]
  -}
  -- test with: chop8 [1,1,1,1,1,1,0,0,0,0,0,1,1,1]
  -- returns: [[1,1,1,1,1,1,0,0],[0,0,0,1,1,1]]
  {-
  chop8 [] = []
  chop8 bits = take 8 bits : chop8 (drop 8 bits)
  -}
  -- WRONG chop8 = unfold [] (drop 8) (take 8)
  -- RIGHT chop8 = unfold null (take 8) (drop 8)
  -- WRONG chop8 = unfold null (drop 8) (take 8)
  -- WRONG chop8 = unfold (const False) (take 8) (drop 8)
  
  -- map :: (a -> b) -> [a] -> [b]
  -- WRONG map f = unfold null (f) tail
  -- WRONG map f = unfold null (f (head)) tail
  -- RIGHT map f = unfold null (f . head) tail
  -- WRONG map f = unfold empty (f . head) tail
  
  -- test with: iterate (*2) 1
  -- iterate :: (a -> a) -> a -> [a]
  -- RIGHT iterate f = unfold (const False) id f
  -- WRONG iterate f = unfold (const False) f f
  -- WRONG iterate f = unfold (const True) id f
  -- WRONG iterate f = unfold (const True) f f
  
  let myfold = myfoldr (-) 0 [1, 2, 3, 4]