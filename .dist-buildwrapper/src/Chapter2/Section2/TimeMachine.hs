{-# LANGUAGE ViewPatterns #-}
module Chapter2.Section2.TimeMachine where
  {- Declare Algebraic Data Type (ADT) representations 
     for 'Client' and 'Person' with Set of Constructors 
     and Arguments to hold type values.
     Add functionality to the ADT with Default Deriving
     to use internal 'print' function to implement Type 
     Class 'Show' an get String representation of values 
     on screen. Example command lin input: 
       Person "Luke" "Schoen" -}
  data Client = GovOrg     String
              | Company    String Integer String String
              | Individual Person Bool
              deriving Show
  data Person = Person String String
              deriving Show
  
  {- Client name Function -}
  {- Pattern matching across some cases. Test input 'clientName (GovOrg "NATO")' -}
  {-
  clientName :: Client -> String
  clientName client = case client of 
                        GovOrg  name       -> name
                        Company name _ _ _ -> name
                        Individual (Person fName lName) _ -> fName ++ " " ++ lName
  -}

  {- Alternatively Encode Pattern Matching directly in the definition -}
  clientName (GovOrg name)                              = name
  clientName (Company name _ _ _)                       = name
  clientName (Individual (Person fName lName) _)        = fName ++ " " ++ lName

{-
  {- Test example: f (Company "A" 5 (Person "John" "Brown" Male) "Director") -}
  f :: Client -> String
  f client = case client of
                        Company _ _ (Person name _) "Boss" -> name + " is the boss"
                        _                                  -> "No boss exists"
  g :: Client -> String
  g client = case client of
               Company _ _ (Person name _) pos ->
                 case pos of "Boss" -> name ++ " is the boss"
               _                    -> "No boss exists"
-}

{- Pattern Matching on List to check whether data is sorted -}
  sorted :: [Integer] -> Bool
  sorted []               = True                {- Empty case -}
  sorted [_]              = True                {- Singleton case -}
  {- As Patterns binds value in match and match its inner components when Multiple elements in list. -}
  {- Compare fst and snd then recursively check if snd and remaining are sorted -} 
  sorted (x : r@(y:_))    = x < y && sorted r   

  responsibility :: Client -> String
  responsibility (Company _ _ _ r) = r
  responsibility _                 = "Unknown"
  
  specialClient :: Client -> Bool
  specialClient (clientName -> "Mr Schoen")    = True
  specialClient (responsibility -> "Director") = True
  specialClient _                              = False