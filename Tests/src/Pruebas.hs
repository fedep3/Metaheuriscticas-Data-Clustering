module Pruebas(
    Options(..),
    genPrograms
) where

import qualified Data.List as DL
import qualified System.Process as SP
import qualified System.FilePath.Posix as SF

data Program = Program { name       :: String
                       , inputFile  :: String
                       , outputFile :: String
                       , fileType   :: FileType
                       , clusters   :: Int
                       , algorithm  :: Algorithm
                       }
    deriving(Eq)

instance Show Program where
    show (Program p i o t k a) = "./" ++ p ++
                                 " -i " ++ i ++
                                 " -o " ++ o ++ (show t) ++
                                 " -k " ++ (show k) ++
                                 (show a)

data FileType = Image | CSV
    deriving(Eq, Read)

instance Show FileType where
    show Image = " --image "
    show CSV   = " --csv "

data Algorithm = Kmeans Improvement
               | Genetic Improvement Individuals Crossover Mutation Tournament
    deriving(Eq)

instance Show Algorithm where
    show (Kmeans r)          = " --kmeans " ++ (show r)
    show (Genetic r i c m t) = " --genetic " ++
                               (show r) ++ (show i) ++
                               (show c) ++ (show m) ++
                               (show t)

data Improvement = Improvement Int
    deriving (Read,Eq)

instance Show Improvement where
    show (Improvement r) = " -l " ++ (show r) ++ " "

data Individuals = Individuals Int
    deriving(Eq)

instance Show Individuals where
    show (Individuals i) = " -I " ++ (show i) ++ " "

data Crossover = Crossover Float
    deriving(Eq)

instance Show Crossover where
    show (Crossover c) = " -c " ++ (show c) ++ " "

data Mutation = Mutation Float
    deriving(Eq)

instance Show Mutation where
    show (Mutation m) = " -m " ++ (show m) ++ " "

data Tournament = Tournament Int
    deriving(Eq)

instance Show Tournament where
    show (Tournament t) = " -t " ++ (show t) ++ " "

data ListGen a = LG (a, a, a)
    deriving (Read, Eq, Show)

listGen :: (Enum a, Num a) => (ListGen a) -> [a]
listGen (LG (a, b, c)) = [a,(a + c)..b]

data AlgOpt = KmeansOpt  (Improvement, Int)
            | GeneticOpt (Improvement,
                          ListGen Int,    -- Individuals.
                          ListGen Float,  -- Crossover rate.
                          ListGen Float,  -- Mutation rate.
                          ListGen Int)    -- Tournament size.
    deriving(Show, Eq, Read)

type File = (String, String, FileType, Int)

type Options = (File, (String, AlgOpt))

genAlgorithm :: AlgOpt -> [Algorithm]
genAlgorithm (KmeansOpt (r, q)) = map (\_ -> Kmeans r) [1..q]
genAlgorithm (GeneticOpt (r, p, c, m, t)) =
    [ (Genetic r (Individuals p') (Crossover (rounder c')) (Mutation (rounder m')) (Tournament t') ) |
      p' <- listGen p,
      c' <- listGen c,
      m' <- listGen m,
      t' <- listGen t,
      p' > t'
    ]

rounder :: Float -> Float
rounder x = if x > 1.0 then (fromIntegral $ floor x) else x

genPrograms :: Options -> [String]
genPrograms ( (p, i, t, k), (s, a) ) = progs
    where a'     = genAlgorithm a                    -- [Algorithm]
          s'     = genOutputNames i s (length a') t  -- [String]
          opts   = zip s' a'                         -- [(String, Algorithm)]
          progs  = [ (show (Program p i o t k alg) ) | (o, alg) <- opts ]

genOutputNames :: String -> String -> Int -> FileType -> [String]
genOutputNames i s n t = map (\x-> (SF.takeDirectory i) ++ "/results/" ++ name ++ (show x) ++ ext) [0..n]
    where ext  = case (SF.hasExtension i) of
                    True  -> SF.takeExtension i
                    False -> if (t == Image) then ".png" else ".result"
          name = s ++ "_" ++ (SF.dropExtension $ SF.takeFileName i) ++ "_"
