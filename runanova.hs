module Main where

import Tests
import System.Environment(getArgs)

runAnova :: IO [Algorithm] -> String -> Int -> IO ()
runAnova = runTestsAnova "mhs" "img/lena.png" PNG Max 9

main :: IO ()
main = do
    (n:_)  <- getArgs
    let n' = (read n :: Int)
    case n' of
        1 -> do
            putStrLn "-- Algoritmo: GA."
            runAnova (genGA 3) "GA" 30
            putStrLn "-- Algoritmo: PSO."
            runAnova (genPSO 3 (255.0, 255.0, 255.0) (0.0,0.0,0.0)) "PSO" 30
            putStrLn "-- Algoritmo: WPSO."
            runAnova (genWPSO 3 (255.0, 255.0, 255.0) (0.0,0.0,0.0)) "WPSO" 30
        2 -> do
            putStrLn "-- Algoritmo: DE."
            runAnova (genDE 20 (255.0, 255.0, 255.0) (0.0,0.0,0.0)) "DE" 30
            putStrLn "-- Algoritmo: SDE."
            runAnova (genSDE 20 (255.0, 255.0, 255.0) (0.0,0.0,0.0)) "SDE" 30
            putStrLn "-- Algoritmo: Bee."
            runAnova (genBee 3) "Bee" 30
        3 -> do
            putStrLn "-- Algoritmo: Ant."
            runAnova (genAnt 100000) "Ant" 30
        _ -> do
            putStrLn "Debe elegir un set de pruebas adecuado."
    
