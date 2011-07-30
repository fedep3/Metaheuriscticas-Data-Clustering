module Main where

import Files
import System.Environment(getArgs)

{- | Genera y corre los programas.
 -   Necesita un archivo de configuración.
 -}
main :: IO ()
main = do
    (n:s:_) <- getArgs
    runTests ".log" n s
    
