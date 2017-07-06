module Syntax where

    data Expression = Atomic String
                    | Apply Expression Expression
                    | Function String Expression
                    deriving (Show)
    
    prettyShow :: Expression -> String
    prettyShow (Atomic n)           = n
    prettyShow (Function f b)       = "(" ++ f ++ " -> " ++ prettyShow b ++ ")"
    prettyShow (Apply f (Atomic n)) = prettyShow f ++ " " ++ n
    prettyShow (Apply f p)          = prettyShow f ++ " (" ++ prettyShow p ++ ")"
