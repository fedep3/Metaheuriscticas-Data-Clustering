module Main where

import Pruebas
import Data.Char(toUpper)
import System.Environment(getArgs)

tests :: String -> [String] -> IO ()
tests _ []     = do
    return ()
tests t (x:xs) = do
    opts     <- readFile x
    let opts' = unwords $ concat $ map words $ lines opts
    case (map toUpper t) of
        "ANOVA" -> do
            runTestsAnova opts'
            putStrLn "----------------"
            tests t xs
        "FINAL" -> do
            runTestsFinal opts'
            putStrLn "----------------"
            tests t xs
        "ALL"   -> do
            runTestsAll opts'
            putStrLn "----------------"
            tests t xs
        "PARSE" -> do
            catch (putStrLn $ show $ (read opts' :: Options))
                  (\_ -> putStrLn $ "Error la leer el archivo " ++ x)
            putStrLn "----------------"
            tests t xs
        t'     -> do
            putStrLn $ "La opción " ++ t' ++ " no existe."
main :: IO ()
main = do
    (t:xs) <- getArgs
    putStrLn "---- INICIO ----"
    tests t xs
    putStrLn "----  FIN   ----"
    
