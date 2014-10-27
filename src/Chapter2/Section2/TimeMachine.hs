{- GHC Extension pragmas -}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter2.Section2.TimeMachine where
  import Data.Char
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
  {-
  greet :: ClientR -> String 
  greet IndividualR { person = PersonR { firstName } } = "Hi " ++ firstName
  greet CompanyR    { clientRName }                    = "Good Day " ++ clientRName
  greet GovOrgR     { }                                = "Welcome"
  -}
  
  {- RecordWildCards GHC Extension used to automatically create bindings for all variables
     not yet mentioned in the pattern
  -}
  greet IndividualR { person = PersonR { .. } } = "Hi " ++ firstName
  greet CompanyR    { .. }                      = "Good Day " ++ clientRName
  greet GovOrgR     { }                         = "Welcome"

  {- Function to ensure a PersonR's first name always starts with capital letter
     using Record updating. PersonR is a binding containing a value of a record type.
  -}
  nameInCapitals :: PersonR -> PersonR
  -- Create exact copy of PersonR where corresponding field has been changed
  -- Record syntax used to Pattern Match on PersonR using x:xs to match a list
  -- 'As Pattern' used to bind the entire value to 'p' for later updating
  -- 'Let' expression used to compute the new name and update 'p' using Record Updating syntax
  nameInCapitals p@(PersonR { firstName = initial:rest }) = 
          let newName = (toUpper initial):rest
          in p { firstName = newName }
  nameInCapitals p@(PersonR { firstName = "" }) = p

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
