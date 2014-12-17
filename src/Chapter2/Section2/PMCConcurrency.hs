-- Poor Mans Concurrency (PMC)

{- 
  Implement a Monad for handling concurrency. Simulate concurrent processes by interleaving them. 
  Interleaving implements concurrency by running the first part of one process, suspending it, 
  and then allowing another process to run, et cetera, et cetera. 
-}

module PMCConcurrency where

  import Control.Monad

  {-
    To suspend a process, we need to grab its "future" and store it away for later use. 
    Continuations are an excellent way of implementing this. We can change a function 
    into continuation passing style by adding an extra parameter, the continuation, 
    that represents the "future" work that needs to be done after this function 
    terminates. Instead of producing its result directly, the function will now 
    apply the continuation to the result. 
    
    Given a computation of type Action, a function that uses a continuation with 
    result type a has the following type: (a -> Action) -> Action
    
    This type can be read as a function that takes as input a continuation function 
    (a -> Action), that specifies how to continue once the result of type a of the 
    current computation is available. An application f c of this type will call c 
    with its result when it becomes available. 

    Unfortunately, because we want to make (a -> Action) -> Action into a monad, we 
    first need to wrap it into a trivial algebraic data type, which we have to wrap 
    and unwrap when implementing the monad operators. You might remember that in the 
    exercises on the Parser monad we used a newtype declaration but since we are 
    ignoring bottoms the difference is insignificant.
    
    As we will emphasize several times in the exercises below, we find it easiest 
    to derive the implementations by ignoring the wrapper (think like a fundamentalist) 
    since that makes "listening to the types" easier, and then add pattern matching 
    and constructor calls to make GHC happy (code like a hacker). This may, or may not, 
    hold for you.
  -}

  data Concurrent a = Concurrent ((a -> Action) -> Action)

  {-
    A process is represented by the following recursive algebraic data type Action that encodes 
    primitive actions that perform a side-effect and then return a continuation action, or the 
    concurrent execution of two actions, or an action that has terminated.
  -}
  data Action 
      = Atom (IO Action)
      | Fork Action Action
      | Stop

  instance Show Action where
      show (Atom x) = "atom"
      show (Fork x y) = "fork " ++ show x ++ " " ++ show y
      show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================

  {- 
    To express the connection between an expression of:
     - type Concurrent a, and one of,
     - type Action
    We define a function that transforms 'a' ((a -> Action) -> Action) into an 
    'Action' that uses Stop :: Action to create the continuation to the 
    'Concurrent a' passed as the first argument to action:
    
      action :: Concurrent a -> Action
    
    The easiest road to implement this function is to initially ignore the Concurrent 
    wrapper, and first define a function:
    
      action :: ((a -> Action) -> Action) -> Action 
    
    Later add the pattern-matching to remove the wrapper and transform a value of 
    type 'Concurrent a' into a value of type ((a -> Action) -> Action) -> Action. As 
    always, let the types guide you. There is only one obvious way to create a value 
    of 'type a -> Action' from the 'value Stop :: Action'. Then when you get a value 
    of type ma :: ((a -> Action) -> Action) there is only one way to combine these 
    two to obtain a value of type Action.

    Implement the function action:
  -}

  action :: Concurrent a -> Action
  -- action = error "You have to implement action"
  action (Concurrent c) = c (const Stop)


-- ===================================
-- Ex. 1
-- ===================================

  stop :: Concurrent a
  -- stop = error "You have to implement stop"
  stop = Concurrent (const Stop)


-- ===================================
-- Ex. 2
-- ===================================

  atom :: IO a -> Concurrent a
  -- atom = error "You have to implement atom"
  atom io = Concurrent $ \c -> Atom (io >>= \a -> return (c a))

-- ===================================
-- Ex. 3
-- ===================================

  fork :: Concurrent a -> Concurrent ()
  --fork = error "You have to implement fork"
  fork a = Concurrent $ \c -> Fork (action a) (c ())

  par :: Concurrent a -> Concurrent a -> Concurrent a
  --par = error "You have to implement par"
  par (Concurrent a) (Concurrent b) = Concurrent $ \c -> Fork (a c) (b c)


-- ===================================
-- Ex. 4
-- ===================================

  instance Monad Concurrent where
      -- (Concurrent f) >>= g = error "You have to implement >>="
      (Concurrent f) >>= g = Concurrent $ \c -> f (\a -> case g a of (Concurrent b) -> b c)
      return x = Concurrent (\c -> c x)


-- ===================================
-- Ex. 5
-- ===================================

  roundRobin :: [Action] -> IO ()
  -- roundRobin = error "You have to implement roundRobin"
  roundRobin []              = return ()
  roundRobin (Atom io  : xs) = io >>= \act -> roundRobin (xs ++ [act])
  roundRobin (Fork a b : xs) = roundRobin (xs ++ [a,b]) -- put actions at the end of the list
  roundRobin (Stop     : xs) = roundRobin xs

-- ===================================
-- Tests
-- ===================================

  ex0 :: Concurrent ()
  ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

  ex1 :: Concurrent ()
  ex1 = do atom (putStr "Haskell")
           fork (loop $ genRandom 7331) 
           loop $ genRandom 42
           atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

  run :: Concurrent a -> IO ()
  run x = roundRobin [action x]

  genRandom :: Int -> [Int]
  genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
  genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
  genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
  genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

  loop :: [Int] -> Concurrent ()
  loop xs = mapM_ (atom . putStr . show) xs
