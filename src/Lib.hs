module Lib
    ( Expression,
      parse,
      run,
      parseAndRun,
      prettyShow,
    ) where

    import Syntax
    import Parser
    import Interpreter

    parseAndRun :: String -> Either String [Expression]
    parseAndRun code = case expr of
            Left err -> Left $ show err
            Right expr -> Right $ run expr
        where expr = parse code
