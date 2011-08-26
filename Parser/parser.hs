module Main where

import System.Environment (getArgs)
import ParserDB

main :: IO ()
main = do
    (x:y:z:_) <- getArgs
    let t = (read x :: AlgType)
    dbh <- connectDB y
    fillDB dbh z t
    
