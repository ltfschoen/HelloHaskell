module Chapter2.Section2.SimpleFunctions where
  {- Given a non-empty list return the first element, otherwise return string "empty" -}
  firstOrEmpty lst = if not (null lst) then head lst else "empty" 
