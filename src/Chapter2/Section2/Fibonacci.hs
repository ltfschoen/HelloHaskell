module Chapter2.Section2.Fibonacci where
  {- Test example: fibonacci 4 -}
  fibonacci :: Integer -> Integer
  fibonacci n = case n of
                  0 -> 0
                  1 -> 1
                  _ -> fibonacci (n-1) + fibonacci (n-2)