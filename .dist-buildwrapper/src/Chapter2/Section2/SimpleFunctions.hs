module Chapter2.Section2.SimpleFunctions where
  {- Given a non-empty list return the first element, otherwise return string "empty" -}
  {- Good practice to annotate function definition with its Type Signature -}
  {- Test with: firstOrEmpty [] or firstOrEmpty ["string"] -}
  firstOrEmpty :: [[Char]] -> [Char]
  firstOrEmpty lst = if not (null lst) then head lst else "empty"
  
  {- Custom Concatenation Function +++ for lists defined using Recursion -}
  {- Note: Recursion uses cases of empty list and where list has initial element and tail -}
  {- Recursion Steps:
       Example: [1,2] +++ [3,4]
       a) Append 2nd List if 1st List empty. Otherwise 1st List (Head) appended with 1st List 
          (Tail only) and 2nd List
         1:([2] +++ [3,4])
       b) Repeat above for Expression within parenthesis until nested Expression 1st List is empty
         1:(2:([] +++ [3,4]))   -- check for emptiness satisfied
         1:(2:[3,4])            -- outcome is base case
       c) Append the final list items, as +++ symbol is no longer apparent
         [1,2,3,4]
  -}
  lst1 +++ lst2 = if null lst1  -- check emptiness
                  then lst2     -- base case
                  else (head lst1) : (tail lst1 +++ lst2)
                  
  {- Custom Reversal Function reverse2 for lists defined using Recursion -}
  reverse2 list = if null list
                  then []
                  else reverse2 (tail list) +++ [head list]