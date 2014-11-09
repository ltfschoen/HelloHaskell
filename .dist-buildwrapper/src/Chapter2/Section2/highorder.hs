module HighOrder where
  import Prelude hiding (
    all)
  -- DON'T UNDERSTAND!!!
  -- Test with 'all even [1,2,3,4]' -- expect return False
  -- Test with 'all odd [1,3,7]' -- expect return True
  all :: (a -> Bool) -> [a] -> Bool
  -- RIGHT all p xs = and (map p xs)
  -- RIGHT all p xs = map p (and xs)
  -- RIGHT all p = and . map p
  -- RIGHT all p = not . any (not . p)
  -- WRONG all p = map p . and
  -- RIGHT all p xs = foldl (&&) True (map p xs)
  -- WRONG all p xs = foldr (&&) False (map p xs) -- returns 
  -- RIGHT all p = foldr (&&) True . map p