{-# LANGUAGE NPlusKPatterns #-}
module Chapter2.Section2.Tut9 where
  import Prelude
  
  import Data.List(genericLength)
  import Data.Char(isDigit)
  import Unsafe.Coerce()
  import Test.QuickCheck()
  --import Test.QuickCheck(arbitrary)
  --import Test.QuickCheck.Property ((==>))
  --import MonadUtils ((<*>), (<$>))
  --import Test.QuickCheck.Arbitrary (Arbitrary)
  
  data Nat = Zero
           | Succ Nat
           deriving Show
          
          
  {- Ex.0 - Select all possible total and terminating implementations of a 
            conversion function natToInteger :: Nat -> Integer that converts 
            any non-bottom, non-partial, finite natural number (note: 0 is a 
            natural number according to this definition), into the corresponding 
            Integer value.
            
            Test with:
              natToInteger Zero               ==> 0
              natToInteger (Succ Zero)        ==> 1
              natToInteger (Succ (Succ Zero)) ==> 2
              etc
  -}
  

  natToInteger :: Nat -> Integer
  
  {- RIGHT   --}           
  natToInteger Zero = 0
  natToInteger (Succ n) = natToInteger n + 1

  {--}
  {- RIGHT
  natToInteger (Succ n) = natToInteger n + 1
  natToInteger Zero = 0
  -}  
  {- WRONG
  natToInteger n = natToInteger n
  -}
  {- RIGHT 
  natToInteger (Succ n) = 1 + natToInteger n
  natToInteger Zero = 0 
  -} 
  {- WRONG - converts all to 1 
  natToInteger Zero = 1
  natToInteger (Succ n) = (1 + natToInteger n) - 1
  -}
  {- RIGHT 
  natToInteger = head . m
    where m Zero = [0]
          m (Succ n) = [sum [x | x <- (1 : m n)]]
  -}
  {- RIGHT          
  --natToInteger :: Nat -> Integer
  natToInteger = \ n -> genericLength [c | c <- show n, c == 'S']
  -}
   
  -- WRONG
  -- Warning: Does not type-check with Integer, but does with Int
  -- https://www.haskell.org/hoogle/?hoogle=length
  --natToInteger :: Nat -> Integer
  --natToInteger :: Nat -> Int
  --natToInteger = \ n -> length [c | c <- show n, c == 'S']

  {- Ex.1 - Select all possible total and terminating implementations of
            a conversion function integerToNat :: Integer -> Nat that 
            converts any non-bottom, non-partial, finite Integer value >= 0,
            into the corresponding Nat value.

            Note: make sure to enable n+k-patterns, if you don't know yet 
            how to do that, Google is your friend.
            Insert above Module {-# LANGUAGE NPlusKPatterns #-}
            
            Test with:
 
  -}
  
  integerToNat :: Integer -> Nat
  
  {- RIGHT - non exhaustive pattern matches -}
  integerToNat 0 = Zero
  integerToNat (n+1) = Succ (integerToNat n)
  {- -}
  
  {-
  integerToNat 0 = Zero
  integerToNat _ = Succ (Succ Zero)
  integerToNat (n+1) = Succ (integerToNat n)
  -}

  {- WRONG - case of 0 should be Zero (to return 0) instead of Succ Zero
  integerToNat 0 = Succ Zero
  integerToNat n = (Succ (integerToNat n))
  -}
  --http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
  {-
  integerToNat = n
    where n 0 = Succ Zero
          n > 0 = (Succ (integerToNat n))
          n < 0 = error "positive n only"
          _ < 0 = error "you've forgotten to include a pattern match"
  -}        
  
  
  --integerToNat n = if n < 0 then error "positive n only" else (Succ (integerToNat n))
  
  -- filter out invalid data before using using ==> implication function 
  -- if negative integers exist for n, then the first integer n to consider
  -- should be 0
  
  -- check if each non-negative integer n is equal to 0
  -- but integerToNat should have type Integer -> Nat
  -- (so this should not even typecheck, unless we made Nat and instance of Num)
  -- i.e. integerToNat 1 == Succ Zero (is not the same as 0 == Zero)
  -- To use QuickCheck with Nats, must first make them instances of Arbitrary
  -- https://stackoverflow.com/questions/5134259/quickcheck-defining-arbitrary-instance-in-terms-of-other-arbitraries
  -- alternatively auto-derive the instances (with TemplateHaskell and auto-deriving)
  -- https://stackoverflow.com/questions/16440208/how-to-generate-arbitrary-instances-of-a-simple-type-for-quickcheck
  -- Test with: quickCheck (\n -> natToInteger (integerToNat n) == n)
  {-
  data C = C
  instance Arbitrary C
    where arbitrary = do a <- 1
                         b <- 2
                         return (C a b)
  -}
  
  --prop_integerToNat' n = not (n < 0) ==> 0 == n
  

  
{- WRONG   
  integerToNat n
    = product [(unsafeCoerce c) :: Integer | c <- show n]
-}

{- WRONG
  integerToNat n = integerToNat n
-}
{-
  *Chapter2.Section2.Tut9> import Test.QuickCheck
  *Chapter2.Section2.Tut9 Test.QuickCheck> quickCheck (\n -> natToInteger (integerToNat n) == n)
  (0 tests)

  --integerToNat n = integerToNat n
  integerToNat n = if n < 0 then error "positive n only" else integerToNat n
-}


  {- RIGHT - non exhaustive pattern matches
  integerToNat (n+1) = Succ (integerToNat n)
  integerToNat 0 = Zero
  -}
  {- RIGHT - non exhaustive pattern matches
  integerToNat (n+1) = let m = integerToNat n in Succ m
  integerToNat 0 = Zero
  -}

  {- WRONG - returns not in scope
  integerToNat = head . m
    where {
          ; m 0 = [0]
          ; m (n + 1) = [sum [x | x <- (1: m n)]]
          }
  -}
   
  {- WRONG - no instances of Num Nat arising from use of genericLength
  --integerToNat :: Integer -> Nat
  integerToNat = \ n -> genericLength [c | c <- show n, isDigit c]
  -}
  
  
  {- Ex.2 - 
    Select all possible total and terminating implementations of an 
    addition function add :: Nat -> Nat -> Nat that adds two non-bottom, 
    non-partial, finite natural numbers m and n, such that 
    natToInteger (add m n) = natToInteger m + natToInteger n.
            
      Test with:
        natToInteger(add(Zero) (Zero)) ==> 0
        natToInteger(add(Zero) (Succ Zero)) ==> 1
        natToInteger(add(Succ Zero) (Zero)) ==> 1
        natToInteger(add(Succ (Succ Zero)) (Zero)) ==> 2
        natToInteger(add(Zero) (Succ (Succ Zero))) ==> 2
        natToInteger(add(Succ (Succ Zero)) (Succ (Succ Zero))) ==> 4
  -}
  
  add :: Nat -> Nat -> Nat
  
{- RIGHT
  add Zero n = n
  add (Succ m) n = Succ (add n m)
-}
{- RIGHT
  add (Succ m) n = Succ (add n m)
  add Zero n = n
-}
{- WRONG
  add Zero n = Zero
  add (Succ m) n = Succ (add m n)  
-}
{- WRONG
  add (Succ m) n = Succ (add m n)
  add Zero n = Zero
-}
{- WRONG  
  add n Zero = Zero
  add n (Succ m) = Succ (add n m)
-}  
{- WRONG   
  add n (Succ m) = Succ (add n m)
  add n Zero = Zero
-}    
{- RIGHT 
  add n Zero = n
  add n (Succ m) = Succ (add m n)
-}
{- RIGHT -}  
  add n (Succ m) = Succ (add m n)
  add n Zero = n

{- Ex.2 - 
   Using recursion, and any correct implementation of the function add 
   from the previous exercise, select from the following options, a total 
   and terminating multiplication function mult :: Nat -> Nat -> Nat that 
   multiplies two non-bottom, non-partial, finite natural numbers m and n, 
   such that natToInteger (mult m n) = natToInteger m * natToInteger n. 
        
   Test with:
     natToInteger(mult(Zero) (Zero)) ==> 0 * 0 = 0
     natToInteger(mult(Zero) (Succ Zero)) ==> 0 * 1 =0
     natToInteger(mult(Succ Zero) (Zero)) ==> 1 * 0 = 0
     natToInteger(mult(Succ (Succ Zero)) (Zero)) ==> 2 * 0 = 0
     natToInteger(mult(Zero) (Succ (Succ Zero))) ==> 0 * 2 = 0
     natToInteger(mult(Succ (Succ Zero)) (Succ (Succ Zero))) ==> 2 * 2 = 4
     natToInteger(mult(Succ (Succ (Succ Zero))) (Succ (Succ Zero))) ==> 3 * 2 = 6
-}

  mult :: Nat -> Nat -> Nat
{- WRONG
  mult Zero Zero = Zero
  mult m (Succ n) = add m (mult m n)
-}
{- RIGHT -}
  mult m Zero = Zero
  mult m (Succ n) = add m (mult m n)  
{--} 
{- WRONG  
  mult m Zero = Zero
  mult m (Succ n) = add n (mult m n)
-}
{- WRONG 
  mult m Zero = Zero
  mult m n = add m (mult m (Succ n))
-}
  
{- Ex.3 - 
   The standard library defines the following algebraic data type to represent 
   the possible comparisons between two values.
   together with a function:
   
     compare :: (Ord a) => a -> a -> Ordering
     
   that decides if a value 
     
     x :: Ord a => a -- is less than (LT), equal to (EQ), or greater than (GT) another value
     y :: Ord a => a
     
   Given the following data type for trees with Integers at the leafs and inside 
   the nodes:
     data Tree = Leaf Integer
               | Node Tree Integer Tree

   Select all correct implementations of the function

     occurs :: Integer -> Tree -> Bool
     
   that decides whether the given Integer occurs in the given Tree. The Tree parameter 
   is a finite, non-partial, non-bottom binary search tree.
   Note: If you haven't encountered case expressions before, Google is your friend.
   
   Example Binary Tree
   t :: Tree
   t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))
   
           5
         /   \
        3     7
      /  \   / \
     1    4  6  9
   
   Bad Example: 
     Tree 4 3 5
     -- Invalid binary search tree since 3 should be to the left of 4
     testtree = Node (Leaf 4) 3 (Leaf 5)
     
     WRONG (not a binary search tree)
      t = Node (Node (Node (Leaf 1) 50 (Leaf 2)) 80 (Node (Leaf 3) 51 (Leaf 4))) 100 (Node (Node (Leaf 5) 60 (Leaf 6)) 90 (Node (Leaf 7) 61 (Leaf 8)))
   
   Test with:
     occurs 3 (Node (Leaf 3) 4 (Leaf 5))
-}
  {-
    data Ordering = LT
                  | EQ
                  | GT

    compare :: (Ord a) => a -> a -> Ordering
  -} 
  
{- 
  data Tree = Leaf Integer
            | Node Tree Integer Tree
  
  occurs :: Integer -> Tree -> Bool
-}  
{- RIGHT 
  occurs m (Leaf n) = m == n
  occurs m (Node l n r)
    = case compare m n of
          LT -> occurs m l
          EQ -> True
          GT -> occurs m r
-}
{- WRONG          
  occurs m (Leaf n) = m == n
  occurs m (Node l n r)
    = case compare m n of
          LT -> occurs m r
          EQ -> True
          GT -> occurs m l  
-}        
{- WRONG
  occurs m (Leaf n) = compare m n
  occurs m (Node l n r)
    = case compare m n of
          LT -> occurs m l
          EQ -> True
          GT -> occurs m r
-}           
{- WRONG           
  occurs m (Leaf n) = m == n
  occurs m (Node l n r)
    = case compare m n of
          LT -> occurs m l
          EQ -> False
          GT -> occurs m r   
-}
{- RIGHT           
  occurs m (Leaf n) = m == n
  occurs m (Node l n r)
    | m == n = True
    | m < n = occurs m l
    | otherwise = occurs m r
-}
{- WRONG (does not find all leaves
  occurs m (Leaf n) = m == n
  occurs m (Node l n r)
    | m == n = True
    | m > n = occurs m l
    | otherwise = occurs m r  
-}
{- WRONG
  occurs m n = m == n
  occurs m (Node l n r)
    | m == n = True
    | m < n = occurs m l
    | otherwise = occurs m r
-}
{- WRONG  
  occurs m n = m == n
  occurs m (Node l n r)
    | m == n = False
    | m < n = occurs m r
    | otherwise = occurs m l
-}

{- Ex.5 - 
   Consider the following type of binary trees, with only values at the leafs:
   
     data Tree = Leaf Integer
               | Node Tree Tree
   
   We say that a tree is balanced if the number of leaves in the left and right 
   subtree of every node differs by at most one, with leaves themselves being 
   trivially balanced. Which option correctly implements

     balanced :: Tree -> Bool

   that decides if a finite, non-partial, non-bottom binary tree is balanced or not?

   Test with:
     leaves (Node (Leaf 3) (Leaf 7)) ==> 10
     balanced (Node (Leaf 3) (Leaf 5)) 
     balanced (Node (Leaf 3) (Leaf 6))
-}

  data Tree = Leaf Integer
            | Node Tree Tree deriving Show

  balanced :: Tree -> Bool
{-
  leaves (Leaf x) = x
  leaves (Node l r) = leaves l + leaves r
  balanced (Leaf _) = True
  balanced (Node l r)
    = abs (leaves l - leaves r) <= 1 || balanced l || balanced r
-}
{-
  leaves (Leaf _) = True
  leaves (Node l r) = leaves l + leaves r
  balanced (Leaf _) = True
  balanced (Node l r)
    = abs (leaves l - leaves r) <= 1
-}
{- WRONG  
  leaves (Leaf _) = True
  leaves (Node l r) = leaves l + leaves r
  balanced (Leaf _) = True
  balanced (Node l r)
    = abs (leaves l + leaves r) <= 1
-}
{- RIGHT -}
  leaves (Leaf _) = 1
  leaves (Node l r) = leaves l + leaves r
  balanced (Leaf _) = True
  balanced (Node l r)
    = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

{- Ex.6 - 
   Given the definition of binary trees from the previous exercise, 
   define a function:
   
   balance :: [Integer] -> Tree

   that converts a finite, non-empty, non-partial, 
   non-bottom list of non-bottom integers into a balanced tree.
   
   Note:
   use
     data Tree = Leaf Integer | Node Tree Tree deriving Show
     
     where deriving Show makes Tree an instance of Show which 
     let's ghci pretty print expressions.
     
   Test with:
   
     balance [1,2]  ==>  Node (Leaf 1) (Leaf 2)
-}   

  balance :: [Integer] -> Tree
{- RIGHT -}
  halve xs = splitAt (length xs `div` 2) xs
  balance [x] = Leaf x
  balance xs = Node (balance ys) (balance zs)
    where (ys, zs) = halve xs

{- WRONG   
  halve xs = splitAt (length xs / 2) xs
  balance [x] = Leaf x
  balance xs = Node (balance ys) (balance zs)
    where (ys, zs) = halve xs 
-}
{- WRONG
  -- Couldn't match expected type ‘[Integer]’
     with actual type ‘([Integer], [Integer])’
  halve xs = splitAt (length xs `div` 2) xs
  balance [x] = Leaf x
  balance xs = Node ys zs
    where (ys, zs) = balance (halve xs)
-}
{- WRONG 
  halve xs = splitAt (length xs `div` 2) xs
  balance x = Leaf x
  balance xs = Node (balance ys) (balance zs)
    where (ys, zs) = halve xs
-}

{- Ex.9 - 
   Given the algebraic data type 
   
     data Maybe a = Nothing | Just a
   
   , pick the correct instance declaration that shows that the type constructor 
   "Maybe" is a Monad. Assume that all values of type Maybe a are finite, non-partial, 
   and non-bottom. You don't have to prove that the Monad laws hold, but use 
   your common sense when picking the right answer.
   
-}
{- RIGHT
  instance Monad Maybe where
    return x = Just x
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x
-} 

{- Ex.10 - 
   Given the list type from the standard Prelude, pick the correct instance 
   declaration that shows that the type constructor [] is a Monad. Assume that
   all values of type [a] are finite, non-partial, and non-bottom. You don't 
   have to prove that the Monad laws hold, but use your common sense when 
   picking the right answer.
   
   Note:  :t []        ==>     [] :: [t]
          :t concat    ==> concat :: [[a]] -> [a]
          :t map       ==>    map :: (a -> b) -> [a] -> [b]
-}  

{- Ex.11 - 
   A monoid is an algebraic structure over a type "a" with a single associative 
   binary operation 
     
     (<>) :: Monoid a => a -> a -> a 
   
   and a neutral element, assuming bottoms don't exist.
   
   mempty :: Monoid a => a
   
   class Monoid a where
     mempty :: a
     (<>) :: a -> a -> a
   
   Note: this class declaration does not enforce the fact that (<>) is 
   associative, and neither that mempty is the neutral element.

   Complete the following instance declaration, assuming values of type a are 
   non-bottom. You don't need to prove that (<>) is associative and that mempty 
   is a neutral element, but use your common sense when picking the correct 
   implementation.
   
   (<>) is "mappend"
-}

{- RIGHT
  instance Monoid [a] where
          mempty = []
          (<>) = (++)
-}
          
{- Ex.12 - 
   A functor is a type constructor with an operation 
   
     fmap :: Functor f => (a -> b) -> f a -> f b 
    
   such that, ignoring the existence of bottoms.
   
     (fmap f) . (fmap g) = fmap (f . g)
     fmap id = id

     class Functor f where
             fmap :: (a -> b) -> f a -> f b
             
   Note: this class declaration does not enforce the fact that fmap satisfies the 
   two laws mentioned above. A value of type Functor f => f a can be considered as a
   "collection" with elements of type "a" and shape "f" (for instance "f" can be [] or Maybe).
   Complete the following instance declaration, assuming bottoms don't exist. You don't 
   need to prove that the two laws hold, but use your common sense when picking the 
   correct implementation.
   
   See http://en.wikibooks.org/wiki/Haskell/The_Functor_class
-}

{- RIGHT
  instance Functor Maybe where
          fmap _ Nothing = Nothing
          fmap f (Just a) = Just (f a)
-}

{- Ex.13 - 
   Given a type constructor "f" such that "Functor f", and an element type "a" such that "Monoid a", 
   we can define a 
   
     function fold :: (Foldable f, Monoid a) => f a -> a 
     
   that folds the values in the argument "collection" using the monoids neutral element "mempty" 
   and the operation <>. 
   Assuming that the "collection" is finite, non-partial, and non-bottom, it doesn't matter 
   from which direction the fold operates because the <> operator is "associative". When the 
   "collection" is empty the result of folding is the neutral element "mempty".

   The module "Data.Foldable" defines the following type class for folding functors with monoid 
   elements:
   
     class (Functor f) => Foldable f where
             fold :: (Monoid m) => f m -> m
   
   Assuming bottom does not exist, complete the following instance declaration for Foldable []:
-}
{- RIGHT
  instance Foldable [] where
          fold = foldr (<>) mempty
-}