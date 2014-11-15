module Parsing where
  import Data.Char
  import Control.Monad
  
{- Implementation of the Parser Combinators -}
  infixr 5 +++

{- The monad of parsers -}

  {- newtype instead of just a type-synonym are wrapped inside
     the data-constructor P as it has to be this way to define
     a monad instance
  -}
  newtype Parser a              =  P (String -> [(a,String)])

  {- Note: Implement this bind (>>=) method so that it is not undefined,
     so that any parser that requires the bind function will work.
     When this is implemented the Console will display *Parsing> as it
     has performed: Prelude> :load
     Test with:     *Parsing> parse (digit) "123"
     Returns:       [('1',"23")]
  -}

  instance Monad Parser where
     {- Note: New versions of GHC reject signatures included here -}
     -- return                  :: a -> Parser a
     return v                   =  P (\inp -> [(v,inp)])
     
     -- (>>=)                   :: Parser a -> (a -> Parser b) -> Parser b
     p >>= f                    =  P (\ inp ->
                                        case parse p inp of
                                            [(v, out)] -> parse (f v) out
                                            [] -> [])

  instance MonadPlus Parser where
     mzero                      =  P (\inp -> [])
     p `mplus` q                =  P (\inp -> case parse p inp of
                                              []        -> parse q inp
                                              [(v,out)] -> [(v,out)])

{- Basic parsers -}

  failure                       :: Parser a
  failure                       =  mzero

  -- Test: parse item "hello"
  -- Returns: [('h', "ello")]
  
  item                          :: Parser Char
  item                          =  P (\inp -> case inp of
                                                 []     -> []
                                                 (x:xs) -> [(x,xs)])

  -- Test: parse (return 1) "hello"
  -- Returns: [(1, "hello")]

  -- Test: parse (item +++ return 'a') "hello"
  -- Returns: [('h',"ello")]
  parse                         :: Parser a -> String -> [(a,String)]
  parse (P p) inp               =  p inp

{- Choice -}

  -- Test: return 1 +++ return 2
  -- Returns: 1 +++ 2
  -- Note: It always succeeds
  
  (+++)                         :: Parser a -> Parser a -> Parser a
  p +++ q                       =  p `mplus` q

{- Derived primitives -}

  sat                           :: (Char -> Bool) -> Parser Char
  sat p                         =  do x <- item
                                      if p x then return x else failure

  digit                         :: Parser Char
  digit                         =  sat isDigit

  lower                         :: Parser Char
  lower                         =  sat isLower

  upper                         :: Parser Char
  upper                         =  sat isUpper

  letter                        :: Parser Char
  letter                        =  sat isAlpha

  alphanum                      :: Parser Char
  alphanum                      =  sat isAlphaNum

  {- Test:
       let test = char 'a' +++ return 'b'
       parse test "yourTestStringHere"
     Returns:
       [('b',"yourTestStringHere")]
  -}
  -- Note: It always succeeds (DOES NOT always succeed with the result value of 'a')

  char                          :: Char -> Parser Char
  char x                        =  sat (== x)

  string                        :: String -> Parser String
  string []                     =  return []
  string (x:xs)                 =  do char x
                                      string xs
                                      return (x:xs)

  many                          :: Parser a -> Parser [a]
  many p                        =  many1 p +++ return []

  many1                         :: Parser a -> Parser [a]
  many1 p                       =  do v  <- p
                                      vs <- many p
                                      return (v:vs)

  ident                         :: Parser String
  ident                         =  do x  <- lower
                                      xs <- many alphanum
                                      return (x:xs)

  {- 
    Parses a sequence of one or more digits 
  -}
  nat                           :: Parser Int
  nat                           =  do xs <- many1 digit
                                      return (read xs)
  {- 
    Parses an integer literal consists of an optional minus sign, 
    followed by a sequence of one or more digits.
    Testing: parse int "-007"
    Result: [(-7,"")] (according to the spec)     
  -}                            
  int                           :: Parser Int
  int                           =  (do char '-'
                                       n <- nat
                                       return (-n))
                                     +++ nat

  space                         :: Parser ()
  space                         =  do many (sat isSpace)
                                      return ()
  {- 
    Parser definition for ordinary Haskell-like comments
    that begin with the symbol -- and extend to end of 
    current line represented by control character '\n'.
    Note that /= is syntax for "not equals"
  -}
  comment                       :: Parser ()
  {- RIGHT -}
  comment                       =  do string "--"
                                      many (sat (/= '\n'))
                                      return ()
  {- 
    Consider expressions built up from non-negative numbers, 
    greater or equal to zero using a subtraction operator
    that associates to the left, where a possible grammar of 
    such expressions would appear as follows:
      exp ::= expr - nat | nat
      nat ::= 0 | 1 | 2 |...
    Note however that this grammar is left-recursive so
    direct transliteration of this grammar into parser 
    combinators would result in a program that doesn't terminate
    To overcome this, choose an iterative implementation of 
    left-associative expressions 
    Test: parse expr "1-2-3-5"
    Returns: [(-9,"")]
  -}
  expr                          :: Parser Int
  {- RIGHT -}
  expr                          = do n <- natural
                                     ns <- many
                                             (do symbol "-"
                                                 natural)
                                     return (foldl (-) n ns)

{- Ignoring spacing -}

  token                         :: Parser a -> Parser a
  token p                       =  do space
                                      v <- p
                                      space
                                      return v

  identifier                    :: Parser String
  identifier                    =  token ident

  natural                       :: Parser Int
  natural                       =  token nat

  integer                       :: Parser Int
  integer                       =  token int

  symbol                        :: String -> Parser String
  symbol xs                     =  token (string xs)
