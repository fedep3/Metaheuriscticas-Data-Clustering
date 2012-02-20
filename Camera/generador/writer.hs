module Main where

import Control.Monad(when, mapM_)
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO(writeFile)
import Data.List

data Alg = Bee | Genetic

data AlgType = B String Int Int Int Int Int Int
             | G String Int Int Int Double Double

toTest :: AlgType
       -> String
toTest (B s a b c d e f) =
    "((\"../mhs\", (InputFile \"" ++ s ++ "\" TIFF), Max, " ++ (show a) ++ ", (Mn [0.0]), (Mx [255.0]) ),\
    \ \n\t(\"Bee\", BeeOpt (Reps 3, \
    \ \n\t\t LG (" ++ (show b) ++ ", " ++ (show b) ++ ", 1),\
    \ \n\t\t LG (" ++ (show c) ++ ", " ++ (show c) ++ ", 1),\
    \ \n\t\t LG (" ++ (show d) ++ ", " ++ (show d) ++ ", 1),\
    \ \n\t\t LG (" ++ (show e) ++ ", " ++ (show e) ++ ", 1),\
    \ \n\t\t LG (" ++ (show f) ++ ", " ++ (show f) ++ ", 1)))\
    \ \n)"
toTest (G s a b c d e)   =
    "((\"../mhs\", (InputFile \"" ++ s ++ "\" TIFF), Max, " ++ (show a) ++ ", (Mn [0.0]), (Mx [255.0]) ),\
    \ \n\t(\"GA\", GAOpt (Reps 3, \
    \ \n\t\t LG (" ++ (show b) ++ ", " ++ (show b) ++ ", 1),\
    \ \n\t\t LG (" ++ (show c) ++ ", " ++ (show c) ++ ", 1),\
    \ \n\t\t LG (" ++ (show d) ++ ", " ++ (show d) ++ ", 1.0),\
    \ \n\t\t LG (" ++ (show e) ++ ", " ++ (show e) ++ ", 1.0)))\
    \ \n)"

toAlgType :: Alg
          -> String
          -> [SqlValue]
          -> AlgType
toAlgType Bee     s [a, b, c, d, e, f] =
    B s (fromSql a) (fromSql b) (fromSql c) (fromSql d) (fromSql e) (fromSql f)
toAlgType Genetic s [a, b, c, d, e]    =
    G s (fromSql a) (fromSql b) (fromSql c) (fromSql d) (fromSql e)


getResult :: Alg
          -> String
getResult Genetic = "SELECT ga_cluster_i, gap_i, gap_tt, gap_pc, gap_pm \
                    \FROM genetico, geneticop \
                    \WHERE ga_type = gap_id and ga_cluster_f >= ? \
                    \GROUP BY ga_type ORDER BY AVG(ga_hib_db) ASC LIMIT ?"
getResult Bee     = "SELECT bee_cluster_i, beep_i, beep_m, beep_e, beep_eb, beep_ob \
                    \FROM abeja, abejap \
                    \WHERE bee_type = beep_id and bee_cluster_f >= ? \
                    \GROUP BY bee_type ORDER BY AVG(bee_hib_db) ASC LIMIT ?"

genQuery :: FilePath  -- ^ Archivo de base de datos de entrada.
         -> FilePath  -- ^ Archivo test de salida.
         -> Alg       -- ^ Tipo de algoritmo (Bee | Genetic)
         -> String    -- ^ Path de la imagen a utilizar.
         -> Int       -- ^ Mínima cantidad de clusters permitidos.
         -> Int       -- ^ Cantidad de resultados o cojuntos de parámetros.
         -> IO ()
genQuery fp ff t s c m = do
    dbh <- connectSqlite3 fp
    xs <- quickQuery' dbh (getResult t) [toSql c, toSql m]
    let l = concat $ intersperse ",\n" $ map (toTest . toAlgType t s) xs
    writeFile ff ("[\n" ++ l ++ "\n]")
    disconnect dbh
