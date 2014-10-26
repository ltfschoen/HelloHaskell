{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
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

  {- Records with set of Fields for access and updating part of the Data Structure
     are defined using the Data Declaration. Test with:
       IndividualR { person = PersonR { lastName = "Smith", firstName = "John" } }
       GovOrgR "ABC Org"
       clientRName (GovOrgR "ABC Org")
       : t duty
  -}
  data ClientR = GovOrgR  { clientRName :: String }
               | CompanyR { clientRName :: String
                          , companyId :: Integer
                          , person :: PersonR
                          , duty :: String }
               | IndividualR { person :: PersonR }
               deriving Show
               
  data PersonR = PersonR { firstName :: String
                         , lastName :: String
                         } deriving Show

  {- Records for fields to bind or match with new pattern (constructor) 
     and list of fields and patterns 
  -}
  {-
  greet :: ClientR -> String
  greet IndividualR { person = PersonR { firstName = fn } } = "Hi " ++ fn
  greet CompanyR    { clientRName = c }                     = "Good Day " ++ c
  greet GovOrgR     { }                                     = "Welcome" -- empty list of fields
  -}

  {- Record Puns GHC Extension used as alternative for Record Matching 
     Note that pragma 'NamedFieldPuns' required 
  -}
  greet :: ClientR -> String 
  greet IndividualR { person = PersonR { firstName } } = "Hi " ++ firstName
  greet CompanyR    { clientRName }                    = "Good Day " ++ clientRName
  greet GovOrgR     { }                                = "Welcome"

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