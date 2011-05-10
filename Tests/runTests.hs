module Main where

import Pruebas
import Data.Char(toUpper)
import System.Environment(getArgs)

main :: IO ()
main = do
    (f:t:_) <- getArgs
    opts    <- readFile f
    let opts' = unwords $ concat $ map words $ lines opts
    case (map toUpper t) of
        "ANOVA" -> do
            runTestsAnova opts'
        "FINAL" -> do
            runTestsFinal opts'
        ""      -> do
            runTestsAll opts'
        _t'     -> do
            putStrLn $ "La opción " ++ t' ++ " no existe."
    
