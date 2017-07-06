module Main where

import System.Environment

import Lib

runFile :: String -> IO ()
runFile filename = do
    code <- readFile filename
    let output = parseAndRun code
    case output of
        Left err     -> putStrLn err
        Right exprls -> do
            mapM_ (\x -> putStrLn $ prettyShow x) exprls
            return ()

main :: IO ()
main = do
    arguments <- getArgs
    case length arguments of
        0 -> putStrLn "ERROR: Please provide a file to evaluate."
        1 -> runFile (arguments !! 0)
        _ -> putStrLn "ERROR: Too many command line arguments."
