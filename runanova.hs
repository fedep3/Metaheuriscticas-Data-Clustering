module Main where

import Tests

main :: IO ()
main = do
    r <- runTests "mhs" "img/lena.png" PNG Max 9 (genGA 3) "GA" 10
    putStrLn $ unlines $ map fst $ stdout r
    
