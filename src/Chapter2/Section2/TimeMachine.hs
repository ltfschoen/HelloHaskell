{- GHC Extension pragmas -}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
-- http://stackoverflow.com/questions/16430025/ambiguous-occurrence
--{-# LANGUAGE NoImplicitPrelude #-}

module Chapter2.Section2.TimeMachine where
  import Data.Char
  import Prelude hiding ((||))
  import Prelude hiding ((&&))
  --import Prelude hiding ((^))
  -- import qualified Prelude as P
  {- Declare Algebraic Data Type (ADT) representations 
     for 'Client' and 'Person' with Set of Constructors 
     and Arguments to hold type values.
     Add functionality to the ADT with Default Deriving
     to use internal 'print' function to implement Type 
     Class 'Show' an get String representation of values 
     on screen. Example command lin input: 
       Person "Luke" "Schoen" -}
  data Client = GovOrg     String
              | Company    String Integer String String
              | Individual Person Bool
              deriving Show
  data Person = Person String String
              deriving Show
  
  {- Client name Function -}
  {- Pattern matching across some cases. Test input 'clientName (GovOrg "NATO")' -}
  {-
  clientName :: Client -> String
  clientName client = case client of 
                        GovOrg  name       -> name
                        Company name _ _ _ -> name
                        Individual (Person fName lName) _ -> fName ++ " " ++ lName
  -}

  {- Records with set of Fields for access and updating part of the Data Structure
     are defined using the Data Declaration. Test with:
       IndividualR { person = PersonR { lastName = "Smith", firstName = "John" } }
       GovOrgR "ABC Org"
       clientRName (GovOrgR "ABC Org")
       : t duty
  -}
  data ClientR = GovOrgR  { clientRName :: String }
               | CompanyR { clientRName :: String
                          , companyId :: Integer
                          , person :: PersonR
                          , duty :: String }
               | IndividualR { person :: PersonR }
               deriving Show
               
  data PersonR = PersonR { firstName :: String
                         , lastName :: String
                         } deriving Show

  {- Records for fields to bind or match with new pattern (constructor) 
     and list of fields and patterns 
  -}
  {-
  greet :: ClientR -> String
  greet IndividualR { person = PersonR { firstName = fn } } = "Hi " ++ fn
  greet CompanyR    { clientRName = c }                     = "Good Day " ++ c
  greet GovOrgR     { }                                     = "Welcome" -- empty list of fields
  -}

  {- Record Puns GHC Extension used as alternative for Record Matching 
     Note that pragma 'NamedFieldPuns' required 
  -}
  {-
  greet :: ClientR -> String 
  greet IndividualR { person = PersonR { firstName } } = "Hi " ++ firstName
  greet CompanyR    { clientRName }                    = "Good Day " ++ clientRName
  greet GovOrgR     { }                                = "Welcome"
  -}
  
  {- RecordWildCards GHC Extension used to automatically create bindings for all variables
     not yet mentioned in the pattern
  -}
  greet IndividualR { person = PersonR { .. } } = "Hi " ++ firstName
  greet CompanyR    { .. }                      = "Good Day " ++ clientRName
  greet GovOrgR     { }                         = "Welcome"

  {- Function to ensure a PersonR's first name always starts with capital letter
     using Record updating. PersonR is a binding containing a value of a record type.
  -}
  nameInCapitals :: PersonR -> PersonR
  -- Create exact copy of PersonR where corresponding field has been changed
  -- Record syntax used to Pattern Match on PersonR using x:xs to match a list
  -- 'As Pattern' used to bind the entire value to 'p' for later updating
  -- 'Let' expression used to compute the new name and update 'p' using Record Updating syntax
  nameInCapitals p@(PersonR { firstName = initial:rest }) = 
          let newName = (toUpper initial):rest
          in p { firstName = newName }
  nameInCapitals p@(PersonR { firstName = "" }) = p

  {- Alternatively Encode Pattern Matching directly in the definition -}
  clientName (GovOrg name)                              = name
  clientName (Company name _ _ _)                       = name
  clientName (Individual (Person fName lName) _)        = fName ++ " " ++ lName

{-
  {- Test example: f (Company "A" 5 (Person "John" "Brown" Male) "Director") -}
  f :: Client -> String
  f client = case client of
                        Company _ _ (Person name _) "Boss" -> name + " is the boss"
                        _                                  -> "No boss exists"
  g :: Client -> String
  g client = case client of
               Company _ _ (Person name _) pos ->
                 case pos of "Boss" -> name ++ " is the boss"
               _                    -> "No boss exists"
-}

{- Pattern Matching on List to check whether data is sorted -}
  sorted :: [Integer] -> Bool
  sorted []               = True                {- Empty case -}
  sorted [_]              = True                {- Singleton case -}
  {- As Patterns binds value in match and match its inner components when Multiple elements in list. -}
  {- Compare fst and snd then recursively check if snd and remaining are sorted -} 
  sorted (x : r@(y:_))    = x < y && sorted r   

  responsibility :: Client -> String
  responsibility (Company _ _ _ r) = r
  responsibility _                 = "Unknown"
  
  specialClient :: Client -> Bool
  specialClient (clientName -> "Mr Schoen")    = True
  specialClient (responsibility -> "Director") = True
  specialClient _                              = False
  
{-
  The type of the following function:
    second xs = head (tail xs)
-}
  {- WRONG second :: Eq a => [a] -> a -- Type -}
  {- RIGHT -}
  second :: [a] -> a
  second xs = head (tail xs)

{-
  The type of the following function:
    swap (x, y) = (y, x)
-} 
  {- RIGHT -}
  swap :: (a, b) -> (b, a)
  swap (x, y) = (y, x)

{-
  The type of the following function:
    pair x y = (x, y)
-}
  {- RIGHT -}
  pair :: a -> b -> (a, b) -- Type
  pair x y = (x, y)

{-
  The type of the following function:
    double x = x * 2
-}
  {- WRONG double :: Int -> Int -- the Type -}
  {- RIGHT double :: Num a => a -> a -- the Type -}
  double :: Num a => a -> a
  double x = x * 2

{-
  The type of the following function:
    palindrome xs = reverse xs == xs
-}
  {- RIGHT -}
  palindrome :: Eq a => [a] -> Bool
  palindrome xs = reverse xs == xs
 
{-
  The type of the following function:
    twice f x = f (f x)
-}
  {- RIGHT -}
{-
  twice :: (a -> a) -> a -> a
  twice f x = f (f x)
-}
 
{-
  The type of the following function:
    f xs = take 3 (reverse xs)
-}
  {- RIGHT -}
{-
  f :: [a] -> [a]
  f xs = take 3 (reverse xs)
-}

-- Select implementations that define the following function 
-- that splits an even-length list in two halves 
--
--  halve :: [a] -> ([a], [a])
{- WRONG
  halve xs = (take n xs, drop n xs)
    where n = length xs / 2
-}
{- WORKS, BUT FOR BOTH EVEN AND ODD LENGTH LISTS
  halve xs = splitAt (length xs `div` 2) xs
-}
{- WORKS, BUT FOR BOTH EVEN AND ODD LENGTH LISTS
  halve xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
    where n = length xs
-}
{- WRONG
  halve xs = splitAt (length xs `div` 2)
-}
{- WRONG - REMOVES AN ELEMENT WHEN PROCESSED
  halve xs = (take n xs, drop (n + 1) xs)
    where n = length xs `div` 2
-}

-- ALTERNATIVE APPROACH TO TESTING
-- let halve xs = (take n xs, drop (n + 1) xs) where n = length xs `div` 2
-- halve [1 .. 10]

{- WORKS, BUT FOR BOTH EVEN AND ODD LENGTH LISTS
  halve xs = splitAt (div (length xs) 2) xs
-}
{- WRONG
  halve xs = splitAt (length xs / 2) xs
-}
{- WORKS, BUT FOR BOTH EVEN AND ODD LENGTH LISTS
  halve xs = (take n xs, drop n xs)
    where n = length xs `div` 2
-}
{-
  safeTail :: [a] -> [a]
-}
{- RIGHT
  safeTail xs = if null xs then [] else tail xs
-}
{- RIGHT
  safeTail [] = []
  safeTail (_ : xs) = xs
-}
{- INVALID FOR [] AND OTHERS
  safeTail (_ : xs)
    | null xs = []
    | otherwise = tail xs
-}
{- RIGHT
  safeTail xs
    | null xs = []
    | otherwise = tail xs
-}
{- INVALID
  safeTail xs = tail xs
  safeTail [] = []
-}
{- RIGHT
  safeTail [] = []
  safeTail xs = tail xs
-}
{- INVALID FOR []
  safeTail [x] = [x]
  safeTail (_ : xs) = xs
-}
{- RIGHT
  safeTail
    = \ xs ->
        case xs of 
            [] -> []
            (_ : xs) -> xs
-}

{- Which of the following definitions is correct for the logical disjunction operator || (i.e. OR)? Choose all correct implementations!
Note: since the || operator is already defined in the Prelude, you have to hide the default implementation in order to test the following code. You can hide the operator by adding the following line to your script:
-}

{- TRUE
  False || False = False 
  _ || _ = True
-}
{- TRUE
  False || b = b
  True || _ = True
-}
{- WRONG AS NOT CORRECT FOR FALSE || FALSE WHICH == FALSE (NOT TRUE)
  b || c
    | b == c = True
    | otherwise = False
-}
{- TRUE
  b || c
    | b == c = b
    | otherwise = True
-}
{- TRUE
  b || False = b
  _ || True = True
-}
{- TRUE
  b || c
    | b == c = c
    | otherwise = True
-}
{- WRONG
  b || True = b
  _ || True = True
-}
{- TRUE
  False || False = False
  False || True = True
  True || False = True
  True || True = True
-}

{- 
Which of the following definitions is correct for the logical conjunction operator && (i.e. AND)? Choose all correct implementations!
Note: since the && operator is already defined in the Prelude, you have to hide the default implementation in order to test the following code. You can hide the operator by adding the following line to your script:
-}

{- 
Show how the curried function definition mult x y z = x * y * z can be understood in terms of lambda expressions.
-}
{- 
 Enter in console:
 let mult x y z = x * y * z
 let mult = \ x -> (\ y -> (\ z -> x * y * z))
 mult 4 6 7
-}
{- 
  The expression f x g y means:
  ((f x) g) y
-}
{-
  The type signature f :: (a -> a) -> a indicates that the function f:
  Takes a function as its argument
-}
{-
  Choose the correct implementation for the function 
  remove :: Int -> [a] -> [a] 
  which takes a number n and a list and 
  removes the element at position n from the list.
  For example: remove 0 [1,2,3,4] = [2,3,4]
-}
{-
  remove :: Int -> [a] -> [a] 
-}
{- WRONG - DOES NOT REMOVE
  remove n xs = take n xs ++ drop n xs
-}
{- WRONG
  remove n xs = drop n xs ++ take n xs
-}
{- WRONG
  remove n xs = take (n + 1) xs ++ drop n xs
-}
{- RIGHT
  remove n xs = take n xs ++ drop (n + 1) xs
-}

{- What is the output of the function call: 
   funct 3 [1, 2, 3, 4, 5, 6, 7]? 
   The function funct is defined as:
   funct :: Int -> [a] -> [a]
   funct x xs = take (x + 1) xs ++ drop x xs
   
   ANS: [1,2,3,4,4,5,6,7]
-}
{-
  funct :: Int -> [a] -> [a]
  funct x xs = take (x + 1) xs ++ drop x xs
-}
{-
  What is the type of the following definition:
  e3 x = x * 2
  Enter the following in GHCi
  let e3 x = x * 2
  :t e3
-}
{-
  Choose a suitable definition for the following type:
  e11 :: (Char, Bool)
  Enter the following in GHCi
  :t '\a'
  returns '\a' :: Char
-}
{-
  Choose a suitable definition for the following type:
  e13 :: Int -> Int -> Int
  Enter the following in GHCi
  let el3 x y = x + y * y
  :t el3
  returns el3 :: Num a => a -> a -> a
-}
{- :t sum [x ^ 2 | x <- [1 .. 100]]
  sum [x ^ 2 | x <- [1 .. 100]] :: (Num a, Enum a) => a
  
  let sum100 = sum [x ^ 2 | x <- [1 .. 100]]
  sum100
-}
{-
  let replicate n a = [a | _ <- [1..n]]
  replicate 6 False
  [False,False,False,False,False,False]
-}
{- WRONG
  pyths n
    = [(x, y, z) | x <- [1 .. n], y <- [1 .. x], z <- [1 .. y], 
       x ^ 2 + y ^ 2 == z ^ 2]
-}
{- WRONG
  pyths n
    = [(x, y, z) | x <- [1 .. n], y <- [x .. n], z <- [y .. n], 
       x ^ 2 + y ^ 2 == z ^ 2]
  returns: [(3,4,5),(6,8,10)]
-}
{- RIGHT
  A triple (x, y, z) of positive integers is pythagorean if x2+y2=z2. 
  Choose the correct implementation for the function 
  pyths :: Int -> [(Int, Int, Int)] that returns the list of all 
  pythagorean triples whose components are at most a given limit.
  
  pyths n
    = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], 
       x ^ 2 + y ^ 2 == z ^ 2]  
  returns: [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-}
{- WRONG
  pyths n = [(x, y, (x ^ 2 + y ^ 2)) | x <- [1 .. n], y <- [1 .. n]]
  returns: [(1,1,2),(1,2,5),(1,3,10),(1,4,17),(1,5,26),(1,6,37),(1,7,50),(1,8,65), ...
-}
{-
  factors :: Integer -> [Integer]
-}
{-
  factors 1 = []
  factors n = let prime = head $ dropWhile ((/= 0) . mod n) [2 .. n]
            in (prime :) $ factors $ div n prime
-}
{- TEXTBOOK
  factors   :: Int -> [Int]
  factors n = [x | x <- [1..n], n `mod` x == 0]
-}
{-           
  factors 1 = []
  factors n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ factors $ div n prime
-}   
{- WRONG
  -- returns [2,3,4,5,7,11,13,17,19,23,29,31,37,41,43,47,...
  -- COURSE VERSION returns [1]
  perfects n = [x | x <- [1 .. n], isPerfect x]
    where isPerfect num = sum (factors num) == num
-}
{- RIGHT
  -- [*** Exception: Prelude.init: empty list
  -- COURSE VERSION [6,28,496]
  perfects n = [x | x <- [1 .. n], isPerfect x] 
    where isPerfect num = sum (init (factors num)) == num
-}
{- WRONG
  -- [*** Exception: Prelude.init: empty list
  -- COURSE VERSION [False,False,False,False,False,True,False
  perfects n = [isPerfect x | x <- [1 .. n]]
    where isPerfect num = sum (init (factors num)) == num
-}
{- WRONG
  -- Couldn't match expected type ‘[Integer]’ with actual type ‘Integer’
  -- In the second argument of ‘(==)’, namely ‘num’
  -- In the expression: init (factors num) == num
  perfects n = [x | x <- [1 .. n], isPerfect x]
    where isPerfect num = init (factors num) == num
-}
{-
  The following list comprehension: [(x,y) | x <- [1,2,3], y <- [4,5,6]]
  can be re-expressed using two or more comprehensions with single generators. 
  Choose the implementation that is equivalent to the one above.
 
  Prelude> :t concat
    concat :: [[a]] -> [a]
  Prelude> [(x,y) | x <- [1,2,3], y <- [4,5,6]]
    [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
  Prelude> concat [[[(x,y)] | x <- [1,2,3]] | y <- [4,5,6]]
    [[(1,4)],[(2,4)],[(3,4)],[(1,5)],[(2,5)],[(3,5)],[(1,6)],[(2,6)],[(3,6)]]
  Prelude> concat [(x,y) | y <- [4,5,6]] | x <- [1,2,3]
    <interactive>:238:31: parse error on input ‘|’
  Prelude> concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]
    [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
  Prelude> [z | z <- [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]]
    [[(1,4),(1,5),(1,6)],[(2,4),(2,5),(2,6)],[(3,4),(3,5),(3,6)]]
-}
{-
  -- positions 0 [1,0,1,0]
  -- returns: [1,3]

  find :: (Eq a) => a -> [(a, b)] -> [b]
  find k t = [v | (k', v) <- t, k == k']

  positions :: (Eq a) => a -> [a] -> [Int]
-}
{- RIGHT
  positions x xs = find x (zip xs [0..n])
    where n = length xs - 1
-}
{-
  positions x xs = find x xs
-}
{-
  positions x xs = find x (zipWith (+) xs [0..n])
    where n = length xs - 1
-}
{-
  positions x xs = find n (zip xs [0..x])
    where n = length xs - 1
-}
{-
    [i | (x', i) <- zip xs [0..n], x == x']
    where n = length xs
-}
{- 
  -- test with: scalarproduct [1, 2, 3] [4, 5, 6]
  -- result: 32
  scalarproduct :: [ Int ] -> [ Int ] -> Int
  -- scalarproduct xs ys = sum [x * y | x <- xs, y <- ys]
  scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]
  -- scalarproduct xs ys = product (zipWith (+) xs ys)
  -- scalarproduct xs ys = sum (product [(x, y) | x <- xs, y <- ys])
-}
{- 
  Caesar cipher program
  modified to handle uppercase letters
  test with shift size 13: encode 13 "Think like a Fundamentalist Code like a Hacker"
-}
{-
  let2int :: Char -> Int
  let2int c = ord c - ord 'a'

  int2let :: Int -> Char
  int2let n = chr (ord 'a' + n)

  shift :: Int -> Char -> Char
  shift n c 
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | otherwise = c

  encode :: Int -> String -> String
  encode n xs = [shift n x | x <- xs]
-}

{-
  Choose the correct definition of the function
  riffle :: [a] -> [a] -> [a] that takes two lists 
  of the same length and interleaves their elements 
  in turn about order.
  Test with: riffle [1,2,3][4,5,6] 
  Expected output: [1,4,2,5,3,6]
-}
{-
  riffle :: [a] -> [a] -> [a]
  riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]
-}
{-
   Choose the correct definition for the function
   divisors :: Int -> [Int] that returns the divisors
   of a natural number.
   Test with: divisors 15
   Expected output: [1, 3, 5, 15]
   The function divides :: Int -> Int -> Bool decides
   if one integer is divisible by another. 
   (Note: You need to implement this function yourself.)
   Example: 
   divides 15 2 = False
   divides 15 3 = True
-}
