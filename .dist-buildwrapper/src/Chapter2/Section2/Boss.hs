module Chapter2.Section2.Boss where
  f :: Client -> String
  f client = case client of
               Company _ _ (Person name _) "Boss" -> name + " is the boss"
               _                                    -> "No boss exists"
  g :: Client -> String
  g client = case client of
               Company _ _ (Person name _) pos ->
                 case pos of "Boss" -> name ++ " is the boss"
               _                    -> "No boss exists"