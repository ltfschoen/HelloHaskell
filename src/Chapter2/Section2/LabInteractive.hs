{-# LANGUAGE TemplateHaskell #-}
module Chapter2.Section2.LabInteractive where
  import Prelude
  
  {- Ex 1: Which of the following implementations defines a
           function putStr' :: String -> IO () that takes a 
           String as its parameter and writes it to the 
           standard output?
           Note: The helper function putChar :: Char -> IO () 
           takes a character as its parameter and writes it to 
           the standard output.
  -}
  putStr' :: String -> IO ()
  
  -- WRONG
  --putStr' [] = return ""
  --putStr' (x : xs) = putChar x >> putStr' xs
  
  -- RIGHT
  putStr' [] = return ()
  putStr' (x : xs) = putChar x >> putStr' xs
  
  -- WRONG
  --putStr' [] = return ()
  --putStr' (x : xs) = putChar x >>= putStr' xs
  
  -- WRONG
  --putStr' [] = return ()
  --putStr' (x : xs) = putStr' xs >>= putChar x
  
  {- Ex 2: Choose all possible implementations for a function 
           putStrLn' :: String -> IO () that takes a String
           parameter and writes it to the standard output, 
           followed by a newline character.
           Assume "fast and loose" reasoning where there are no 
           bottoms involved, and all functions are total, and all 
           values are finite.
  -}
  
  putStrLn' :: String -> IO ()
  
  -- RIGHT
  putStrLn' [] = putChar '\n'
  putStrLn' xs = putStr' xs >> putStrLn' ""
 
  -- RIGHT 
  --putStrLn' [] = putChar '\n'
  --putStrLn' xs = putStr' xs >> putChar '\n'
 
  -- RIGHT  
  --putStrLn' [] = putChar '\n'
  --putStrLn' xs = putStr' xs >>= \ x -> putChar '\n'  

  -- WRONG
  --putStrLn' [] = putChar '\n'
  --putStrLn' xs = putStr' xs >> \ x -> putChar '\n'   
  
  -- RIGHT
  -- NOTE: "" instead of '' has been used!!!
  --putStrLn' [] = putChar '\n'
  --putStrLn' xs = putStr' xs >> putStr' "\n"
  
  -- WRONG
  --putStrLn' [] = putChar '\n'
  --putStrLn' xs = putStr' xs >> putStrLn' "\n"

  -- WRONG 
  --putStrLn' [] = return ""
  --putStrLn' xs = putStrLn' xs >> putStr' "\n" 

  -- WRONG 
  --putStrLn' [] = putChar "\n"
  --putStrLn' xs = putStr' xs >> putChar '\n'
  
  {- Ex 3: Which of the following implementation defines a function 
           getLine' :: IO String that reads a line, up to the first 
           \n character, from the standard input?
           Note: The helper function getChar :: IO Char reads a single 
           character from the standard input.
           Test #1:
             Type: getLine'
             Wait for prompt on new line to appear
             Type: ab c\nabc
             It should only return: "abc"
  -}
  
  -- WRONG - returns "abc\\nabc" for INPUT abc\nabc
  -- WRONG - returns "ab" for INPUT ab c\nabc
  
  {-
  getLine' :: IO String
  
  getLine' = get ""
  
  get :: String -> IO String
  get xs
    = do x <- getChar
         case x of
             ' ' -> return xs
             '\n' -> return xs
             _ -> get (xs ++ [x])
  -}
  
  -- WRONG - returns "cban\\cba"
  {-
  getLine' :: IO String
  
  getLine' = get ""
  
  get :: String -> IO String
  get xs
    = do x <- getChar
         case x of
             '\n' -> return xs
             _ -> get (x : xs)
  -}
  
  --  - returns "abc\\nabc"
  -- RIGHT - returns "ab c\\nabc" for INPUT ab c\nabc
  getLine' :: IO String
  
  getLine' = get []
  
  get :: String -> IO String
  get xs
    = do x <- getChar
         case x of
             '\n' -> return xs
             _ -> get (xs ++ [x])
  
  
  -- WRONG - returns "\nabc\\nabc"
  {-
  getLine' :: IO String
  
  getLine' = get []
  
  get :: String -> IO String
  get xs
    = do x <- getChar
         case x of
             '\n' -> return (x : xs)
             _ -> get (xs ++ [x])
  -}
  
   
  {- Ex 4: Which of the following implementations defines a function 
           interact' :: (String -> String) -> IO () that takes as its argument a function 
           of type String -> String, and reads a line from the standard input, 
           and passes it to this function, and then prints the resulting output followed 
           by a newline on the standard output?
           
           Note: Quick tip: the ' is there because sequence_ is a built-in function. 
           You can use that to see what the behaviour should be.
           
           Prelude> :t interact
           interact :: (String -> String) -> IO ()
  -}
  
  -- NO IDEA
  {-
  interact' :: (String -> String) -> IO ()
  
  interact' f
    = do input <- getLine'
         putStrLn' (f input)
  -}
  
  {- Ex. 5: Choose all possible implementations of the function 
            sequence_' :: Monad m => [m a] -> m () that takes a finite, non-partial, 
            list of non-bottom, monadic values, and evaluates them in sequence, 
            from left to right, ignoring all (intermediate) results?

            Hint: Make sure you type in all these definitions in GHCi and play around 
            with a variety of possible input.
            
            Test with: sequence_' [(>4), (<10), odd] 7
                       sequence_' [putChar 'a', putChar 'b', putChar 'c', putChar 'd']
  -}

  sequence_' :: Monad m => [m a] -> m ()
  
  -- WRONG
  {- 
  sequence_' [] = return []
  sequence_' (m : ms) = m >> \ _ -> sequence_' ms
  -}

  {-
    Test output:
    
  <interactive>:2:14: Warning:
    Defaulting the following constraint(s) to type ‘Integer’
      (Ord a0) arising from a use of ‘>’ at <interactive>:2:14
      (Num a0) arising from the literal ‘4’ at <interactive>:2:15
      (Integral a0) arising from a use of ‘odd’ at <interactive>:2:26-28
    In the expression: (> 4)
    In the first argument of ‘sequence_'’, namely
      ‘[(> 4), (< 10), odd]’
    In the expression: sequence_' [(> 4), (< 10), odd] 7
  ()
  -}

  -- MAYBE
  {- 
    Check Type Signature
    Prelude> let sequence_' [] = return ()
    Prelude> let sequence_' (m : ms) = (foldl (>>) m ms) >> return ()
    Prelude> :t sequence_'
    sequence_' :: Monad m => [m a] -> m ()
  -}
  
  sequence_' [] = return ()
  sequence_' (m : ms) = (foldl (>>) m ms) >> return ()
  
  
  -- WRONG
  {-
    Check Type Signature:
    Prelude> let sequence_' ms = foldl (>>) (return ()) ms
    Prelude> :t sequence_'
    sequence_' :: Monad m => [m ()] -> m ()
  -}
  {-
  sequence_' ms = foldl (>>) (return ()) ms
  -}
  
  {-
    Test output:

  <interactive>:9:14: Warning:
    Defaulting the following constraint(s) to type ‘Integer’
      (Ord a0) arising from a use of ‘>’ at <interactive>:9:14
      (Num a0) arising from the literal ‘4’ at <interactive>:9:16
      (Integral a0) arising from a use of ‘odd’ at <interactive>:9:28-30
    In the expression: (> 4)
    In the first argument of ‘sequence_'’, namely
      ‘[(> 4), (< 10), odd]’
    In the expression: sequence_' [(> 4), (< 10), odd] 7
  ()
  -}
  
  -- WRONG !! MAYBE
  {- 
    Check Type:
    Prelude> letsequence_' (m : ms) = m >> sequence_' ms
    Prelude> let sequence_' (m : ms) = m >> sequence_' ms
    Prelude> :t sequence_'
    sequence_' :: Monad m => [m a] -> m b
  -}
  {-
  sequence_' [] = return ()
  sequence_' (m : ms) = m >> sequence_' ms
  -}
  
  {-
    Test output:
    
  <interactive>:19:14: Warning:
    Defaulting the following constraint(s) to type ‘Integer’
      (Ord a0) arising from a use of ‘>’ at <interactive>:19:14
      (Num a0) arising from the literal ‘4’ at <interactive>:19:16
      (Integral a0) arising from a use of ‘odd’ at <interactive>:19:28-30
    In the expression: (> 4)
    In the first argument of ‘sequence_'’, namely
      ‘[(> 4), (< 10), odd]’
    In the expression: sequence_' [(> 4), (< 10), odd] 7
  ()
  -}
  
  -- WRONG !! MAYBE
  {-
    Check Type:
    Prelude> let sequence_' [] = return ()
    Prelude> let sequence_' (m : ms) = m >>= \ _ -> sequence_' ms
    Prelude> :t sequence_'
    sequence_' :: Monad m => [m a] -> m b
  -}
  {-
  sequence_' [] = return ()
  sequence_' (m : ms) = m >>= \ _ -> sequence_' ms
  -}
  
  {- 
    Test output:
    
    Couldn't match type ‘m ()’ with ‘()’
    Expected type: m a -> m () -> m ()
      Actual type: m a -> (a -> m ()) -> m ()
  -}
  
  -- WRONG
  {-
  sequence_' ms = foldr (>>=) (return ()) ms
  -}

  {-
    Test output:
    
  <interactive>:29:14: Warning:
    Defaulting the following constraint(s) to type ‘Integer’
      (Ord a0) arising from a use of ‘>’ at <interactive>:29:14
      (Num a0) arising from the literal ‘4’ at <interactive>:29:16
      (Integral a0) arising from a use of ‘odd’ at <interactive>:29:28-30
    In the expression: (> 4)
    In the first argument of ‘sequence_'’, namely
      ‘[(> 4), (< 10), odd]’
    In the expression: sequence_' [(> 4), (< 10), odd] 7
  ()
  -}

  -- MAYBE
  {- 
    Check Type:
    Prelude> let sequence_' ms = foldr (>>) (return ()) ms
    Prelude> :t sequence_'
    sequence_' :: Monad m => [m a] -> m ()
  -}
  {-
  sequence_' ms = foldr (>>) (return ()) ms
  -}
  
  -- WRONG
  -- Couldn't match expected type ‘()’ with actual type ‘[t0]’
  --sequence_' ms = foldr (>>=) (return []) ms
  
  {- Ex. 6: Choose all possible implementations of the function 
            sequence' :: Monad m => [m a] -> m [a] that takes a finite, non-partial, 
            list of non-bottom, monadic values, and evaluates them in sequence, 
            from left to right, collecting all (intermediate) results into a list?
            Hint: Make sure you type in all these definitions in GHCi and play around 
            with a variety of possible input.
            
            Test with: sequence' [(>4), (<10), odd] 7
  -}
  
  sequence' :: Monad m => [m a] -> m [a]
  
  
  {- RIGHT -}

  sequence' [] = return []
  sequence' (m : ms)
    = m >>=
        \ a ->
          do as <- sequence' ms
             return (a : as)


  {- WRONG
  sequence' ms = foldr func (return ()) ms
    where
          func :: (Monad m) => m a -> m [a] -> m [a]
          func m acc
            = do x <- m
                 xs <- acc
                 return (x : xs)
  -}

  {- WRONG
  sequence' ms = foldr func (return []) ms
    where
          func :: (Monad m) => m a -> m [a] -> m [a]
          func m acc = m : acc
  -}

  {- WRONG
  sequence' [] = return []
  sequence' (m : ms) = return (a : as)
      where
          a <- m
          as <- sequence' ms
  -}

  {- RIGHT
  sequence' ms = foldr func (return []) ms
      where
          func :: (Monad m) => m a -> m [a] -> m [a]
          func m acc 
            = do x <- m
                 xs <- acc
                 return (x : xs)
  -}

  {- WRONG
  sequence' [] = return []
  sequence' (m : ms) 
    = m >>
        \ a ->
          do as <- sequence' ms
             return (a : as)
  -}           

  {- WRONG
  sequence' [] = return []
  sequence' (m : ms) = m >>= \a ->
      as <- sequence' ms
      return (a : as)
  -}    
    
  {- RIGHT
  sequence' [] = return []
  sequence' (m : ms) 
    = do a <- m
         as <- sequence' ms
         return (a : as)
  -}

  {- Ex. 7: 
     Choose all possible implementations of a function 
     mapM' :: Monad m => (a -> m b) -> [a] -> m [b] which takes a non-bottom 
     function of type a -> m b, and a finite, non-partial list of non-bottom 
     elements of type a and (similarly to map) applies the function to every 
     element of the list, but produces the resulting list wrapped inside a monadic 
     action?
     Note: mapM' must preserve the order of the elements of the input list.
     Hint: Make sure you try each of these options in GHCi and play around with 
     a variety of inputs to experiment with the behaviour. It's easiest to use the 
     IO Monad for the function passed into mapM'
     
     Test with: 
       mapM' reverse ["ab","3","12"]
       
     Expect: 
       ["b32","b31","a32","a31"]
       
     Also test with:
       mapM' (putChar) ['a', 'b', 'c']
     Gives: 
       abc[(),(),()]
       
     Also test with:
       mapM' (\x -> putChar x >> return x) ['a', 'b', 'c']
     Gives: 
       abc"abc"
  -}  
  
  mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
  
  {- RIGHT -}
  mapM' f as = sequence' (map f as)

  
  {- RIGHT 
  mapM' f [] = return []
  mapM' f (a : as)
   = f a >>= \ b -> mapM' f as >>= \ bs -> return (b : bs)
  -}
  {- WRONG 
  mapM' f as = sequence_' (map f as)
  -} 
  {- WRONG   
  mapM' f [] = return []
  mapM' f (a : as)
    = f a >> \ b -> mapM' f as >> \ bs -> return (b : bs)
  -} 
  {- WRONG    
  mapM' f [] = return []
  mapM' f (a : as) =
      do
          f a -> b
          mapM' f as -> bs
          return (b : bs)
  -}          
  {- RIGHT         
  mapM' f [] = return []
  mapM' f (a : as)
    = do b <- f a
         bs <- mapM' f as
         return (b : bs)
  -}
  {- RIGHT
  mapM' f [] = return []
  mapM' f (a : as)
    = f a >>=
        \ b ->
          do bs <- mapM' f as
             return (b : bs)
  -}
  {- WRONG          
  mapM' f [] = return []
  mapM' f (a : as )
    = f a >>=
        \ b ->
          do bs <- mapM' f as
             return (bs ++ [b])
  -}

  {- Ex. 8: 
     Which of the following definitions implements the function 
     filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a], that takes a 
     "predicate" of type Monad m => a -> m Bool and uses this to filter a 
     finite, non-partial list of non-bottom elements of type a.
     Note: filterM' must preserve the order of the elements of the input list, 
     as far as they appear in the result.

     Test with: 
       filterM' True [False,True,False]
       filterM (const [True, False])
     Expect: 
       .
  -}  

  -- NO IDEA!!!

  --filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
  {-
  filterM' _ [] = return []
  filterM' p (x : xs)
    = do flag <- p x
         ys <- filterM' p xs
         return (x : ys)
  -}
  {-      
  filterM' _ [] = return []
  filterM' p (x : xs)
    = do flag <- p x
         ys <- filterM' p xs
         if flag then return (x : ys) else return ys
  -}      
  {- WRONG
  filterM' _ [] = return []
  filterM' p (x : xs)
    = do ys <- filterM' p xs
         if p x then return (x : ys) else return ys
  -}
  {-   
  filterM' _ [] = return []
  filterM' p (x : xs)
    = do flag <- p x
         ys <- filterM' p xs
         if flag then return ys else return (x : ys)
  -}
  
  {- Ex. 9
  
     Implement the function foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a 
     that takes an accumulation function a -> b -> m a, and a seed of type a and 
     left folds a finite, non-partial list of non-bottom elements of type b into a 
     single result of type m a  
     
     Hint: The recursive structure of foldLeftM looks as follows:

     foldLeftM f a [] = ... a ...
     foldLeftM f a (x:xs) = foldLeftM f (... f ... a ... (>>=) ...) xs 
     
     Remember the definition of foldl:

     foldl f a [] = ... a ...
     foldl f a (x:xs) = foldl f (... f ... a ...) xs 
 
     What is the result of evaluating the expression:

        haskellhaskell 
        lleksahhaskell 
        haskellhaskellhaskell 
        haskelllleksahhaskell

  -}

  {- Proposed approach:
    Apply the function with the default accumulator 
    and the leftmost element of the list 
    and bind its return (to get rid of the monadic)
    then proceed to apply the function recursively, 
    passing the same f, but instead of the default accumulator
    pass it b and the remainder of the list.
    Noting to 'return a' for the base case
  -}
  
  --foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
  
  --foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r
  
  
  {- Ex. 10
     Implement the function foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b 
     which is like to foldLeftM from the previous exercise, except that it folds a finite, 
     non-partial list of non-bottom elements of type a into a single monadic value of type m b.
        Hint: look up the definition of foldr.
        What is the result of evaluating the expression:
        
          ]9,7,5,3,1[[1,3,5,7,9] 
          [9,7,5,3,1][1,3,5,7,9] 
          [1,3,5,7,9][9,7,5,3,1] 
          [1,3,5,7,9][1,3,5,7,9]
        
        Note: A simple test like the following won't reveal the flaw in the code
          foldLeftM (\a b -> Just (a + b)) 0 [1..10]
          
        Proper test in GHCi
          foldRightM' (\a b -> putChar a >> return (a : b)) [] (show [1,3..10]) >>= \r -> putStrLn r
        
        Returns: 
          ]9,7,5,3,1[[1,3,5,7,9]
  -}
  
  foldRightM' :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
  
  -- carefully compare with the implementation of foldr
  -- foldRightM' |operator| |init_value| |list|
  -- Note: the code must be associating to the right and implementing the right fold correctly
  foldRightM' f d []     = return d
  foldRightM' f d (x:xs) = (\z -> f x z) <<= foldRightM' f d xs
    where (<<=) = flip (>>=)

  
  --
  
  {- Ex. 11
     Choose all possible implementations that define a function 
     liftM :: Monad m => (a -> b) -> m a -> m b that takes a 
     function of type a -> b and "maps" it over a non-bottom 
     monadic value of type m a to produce a value of type m b?
  
  -} 
  liftM :: Monad m => (a -> b) -> m a -> m b
  
  -- liftM (+1) [1,2]   ===> [2,3]
  -- liftM (+1) []      ===> []
  
  -- RIGHT
  
  liftM f m
    = do x <- m
         return (f x)
  
  
  -- WRONG
  --liftM f m = m >>= \ a -> f a
  
  -- RIGHT
  --liftM f m = m >>= \ a -> return (f a)
  
  -- WRONG  
  --liftM f m = return (f m)
  
  {- WRONG - doesn't map correctly
    liftM (+1) [1,2] ===> [2,2,3,3]
  -}  
  --liftM f m = m >>= \ a -> m >>= \ b -> return (f a)

  {- WRONG - doesn't map correctly
    liftM (+1) [1,2] ===> [2,3,2,3]
  -}  
  --liftM f m = m >>= \ a -> m >>= \ b -> return (f b)
  
  --WRONG   
  --liftM f m = mapM f [m]

  --WRONG 
  --liftM f m = m >> \ a -> return (f a) 

