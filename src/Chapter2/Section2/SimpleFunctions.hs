module Chapter2.Section2.SimpleFunctions where
  {- Given a non-empty list return the first element, otherwise return string "empty" -}
  {- Good practice to annotate function definition with its Type Signature -}
  {- Test with: firstOrEmpty [] or firstOrEmpty ["string"] -}
  firstOrEmpty :: [[Char]] -> [Char]
  firstOrEmpty lst = if not (null lst) then head lst else "empty"