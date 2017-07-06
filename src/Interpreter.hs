module Interpreter where

    import System.Environment
    import Control.Applicative
    
    import Syntax

    rename :: String -> Expression -> Expression
    rename str (Atomic name)
        | str == name              = Atomic (str ++ "_")
        | otherwise                = Atomic name
    rename str (Apply f p)         = Apply (rename str f) (rename str p)
    rename str (Function f b)
        | str == f                 = Function f b
        | otherwise                = Function f (rename str b)

    replace :: String -> Expression -> Expression -> Expression
    replace str expr (Atomic name)
        | str == name             = expr
        | otherwise               = Atomic name
    replace str expr (Apply f p)  = Apply (replace str expr f) (replace str expr p)
    replace str expr (Function f b)
        | str == f                = Function f b
        | otherwise               = Function f (replace str (rename f expr) b)

    step :: Expression -> Maybe Expression
    step (Atomic n)               = Nothing
    step (Function f b)           = fmap (\x -> Function f x) $ step b
    step (Apply (Function f b) p) = Just $ replace f p b
    step (Apply f p)              = stepf <|> stepp
        where
            stepf = fmap (\x -> Apply x p) $ step f
            stepp = fmap (\x -> Apply f x) $ step p

    run :: Expression -> [Expression]
    run expr = case dexpr of
            Nothing -> []
            Just ex -> ex:(run ex)
        where dexpr = step expr
