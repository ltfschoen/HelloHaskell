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
  --putStrLn' [] = putChar '\n'
  --putStrLn' xs = putStr' xs >> putStrLn' ""
 
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