module Pruebas(
    Results(..),
    runTestsAnova,
    runTestsFinal,
    runTestsAll
) where

import qualified Data.List as DL
import qualified System.Process as SP

-- | Ejecuta el programa y devuelve la salida del mismo.
run :: Program          -- * Programa a ejecutar
    -> IO String        -- * Salida de la ejecución del programa.
run p = do
    let (e:a) = words $ show p
    SP.readProcess e a ""

-- | Resultado de cada prueba.
data Results = Results { prog   :: String
                       , file   :: String
                       , stdout :: [(String, String)]
                       }
    deriving(Show)

-- | Corre las pruebas de acuerdo a una acción monádica generadora.
runTests :: String         -- * Instrucciones de corrida.
         -> IO Results     -- * Lista de resultados.
runTests o = do
    let o'     = (read o :: Options)
    let p      = genPrograms o'
    let (n, InputFile i _) = (\p -> (name p, inputFile p)) $ head p
    let hAnova = (headerAnova . algorithm) $ head p
    let lanova = (hAnova:(map (getAnova . algorithm) p))
    lstdout   <- mapM run p
    let lstdout' = zip lanova ("":(map filterStdout lstdout))
    let l      = ((head lstdout') : (map helper $ tail lstdout'))
    return (Results n i l)

-- | Filtra la salida para tener la última línea.
filterStdout :: String -> String
filterStdout = unlines . filter ((/=) '-' . head) . lines

helper :: (String, String) -> (String, String)
helper (xs, ys) = (s ++ "\t" ++ xs, ys)
    where (s:_, _) = DL.foldl' (helperBinary ',') ([], "") ys

helperBinary :: Char -> ([String], String) -> Char -> ([String], String)
helperBinary c (x, w) y =
    if (c == y) then ((reverse w):x, [])
                else (x,y:w)  

-- | Corre las pruebas ANOVA de acuerdo a las instrucciones dadas.
runTestsAnova :: String -- * Intrucciones de corrida.
              -> IO ()
runTestsAnova o = do
    r <- runTests o 
    putStrLn $ unlines $ map fst $ stdout r

-- | Corre las pruebas de acuerdo a las instrucciones dadas.
runTestsFinal :: String -- * Intrucciones de corrida.
              -> IO ()
runTestsFinal o = do
    r <- runTests o
    putStrLn $ unlines $ map snd $ stdout r

runTestsAll :: String   -- * Instrucciones de corrida.
            -> IO ()
runTestsAll o = do
    r <- runTests o
    putStrLn $ show r

-- | Genera el header para generar la tabla ANOVA.
headerAnova :: Algorithm -> String
headerAnova (Kmeans _)       = ""
headerAnova (GA _ _ _ _)     = "ofdb\ti\ttt\tpc\tpm"
headerAnova (PSO _ _ _ _)    = "ofdb\ti\tc1\tc2\tw\tvmx"
headerAnova (WPSO _ _ _ _ _) = "ofdb\ti\tc1\tc2\tw\tvmx\tw1\tw2\tw3"
headerAnova (DE _ _ _)       = "ofdb\ti\tw1\tw2\tw3"
headerAnova (SDE _ _ _ _ _)  = "ofdb\ti\tw1\tw2\tw3\tf\tpc"
headerAnova (Ant _ _ _)      = "ofdb\ti\talpha"
headerAnova (Bee _ _)        = "ofdb\ti\tm\te\teb\tob"

-- | Genera una línea apta para generar una tabla ANOVA.
getAnova :: Algorithm   -- * Algoritmo.
         -> String      -- * String con los valores relevantes.
getAnova (Kmeans _) =
    ""
getAnova (GA _ (IGA i t) (Pc pc) (Pm pm)) =
    (show i) ++ "\t" ++ (show t) ++ "\t" ++ (show pc) ++ "\t" ++
    (show pm)
getAnova (PSO _ (I i) (Velocity c1 c2 w) (VMax vmx)) =
    (show i) ++ "\t" ++ (show c1) ++ "\t" ++ (show c2) ++ "\t" ++
    (show w) ++ "\t" ++ (show vmx)
getAnova (WPSO _ (I i) (Velocity c1 c2 w) (VMax vmx) (Weight w1 w2 w3)) =
    (show i) ++ "\t" ++ (show c1) ++ "\t" ++ (show c2) ++ "\t" ++
    (show w) ++ "\t" ++ (show vmx) ++ "\t" ++ (show w1) ++ "\t" ++
    (show w2) ++ "\t" ++ (show w3)
getAnova (DE _ (I i) (Weight w1 w2 w3)) =
    (show i) ++ "\t" ++ (show w1) ++ "\t" ++ (show w2) ++ "\t" ++
    (show w3)
getAnova (SDE _ (I i) (Weight w1 w2 w3) (Scale s) (Pc pc)) =
    (show i) ++ "\t" ++ (show w1) ++ "\t" ++ (show w2) ++ "\t" ++
    (show w3) ++ "\t" ++ (show s) ++ "\t" ++ (show pc)
getAnova (Ant _ (IAnt i) a) =
    (show i) ++ "\t" ++ (show a)
getAnova (Bee _ (IBee i m e eb ob)) =
    (show i) ++ "\t" ++ (show m) ++ "\t" ++ (show e) ++ "\t" ++
    (show eb) ++ "\t" ++ (show ob)

-- | Tipo para representar una llamada al programa.
data Program = Program { name       :: String
                       , inputFile  :: InputFile
                       , outputFile :: OutputFile
                       , oftype     :: OFType
                       , clusters   :: Clusters
                       , minVec     :: Mn
                       , maxVec     :: Mx
                       , algorithm  :: Algorithm
                       }
    deriving(Eq)

-- | Instancia de Show para Program.
instance Show Program where
    show (Program p i o t k mn mx a) = "./" ++ p ++ " -d " ++
                                       (show i) ++ (show o) ++
                                       (show t) ++ (show k) ++
                                       (show mn) ++ (show mx) ++
                                       (show a)

-- | Tipo de dato que representa a un algoritmo.
data Algorithm = Kmeans Repetitions
               | GA Repetitions PopulationGA Pc Pm
               | PSO Repetitions Population Velocity VMax
               | WPSO Repetitions Population Velocity VMax Weight
               | DE Iterations Population Weight
               | SDE Iterations Population Weight Scale Pc
               | Ant Iterations PopulationAnt Float
               | Bee Repetitions PopulationBee
    deriving(Eq)

-- | Instancia de Show para Algorithm.
instance Show Algorithm where
    show (Kmeans r)        = " --a Kmeans " ++ (show r)
    show (GA r i pc pm)    = " --a GA " ++
                                   (show r) ++ (show i) ++
                                   (show pc) ++ (show pm)
    show (PSO r i v vx)    = " --a PSO " ++
                                   (show r)  ++ (show i) ++
                                   (show v)  ++ (show vx)
    show (WPSO r i v vx w) = " --a PSO" ++
                                   (show w) ++ (show r) ++
                                   (show i) ++ (show v) ++
                                   (show vx)
    show (DE r i w)        = " --a DE " ++ (show r) ++
                                   (show i) ++ (show w)
    show (SDE r i w s pc)  = " --a DE " ++ (show r) ++
                                   (show i) ++ (show w) ++
                                   (show s) ++ (show pc)
    show (Ant r i a)             = " --a Ant " ++ (show r) ++
                                   (show i) ++ (show a) ++ " "
    show (Bee r i)               = " --a Bee " ++ (show r) ++
                                   (show i)
    

-- | Archivo de entrada del algoritmo.
data InputFile = InputFile String FileType
    deriving(Read, Eq)

-- | Instancia de Show para InputFile.
instance Show InputFile where
    show (InputFile s ft) = " --fi " ++ s ++
                            " --t " ++ (show ft) ++
                            " "
-- | Archivo de salida del algoritmo
data OutputFile = OutputFile String
    deriving(Eq)

-- | Instancia de Show para OutputFile.
instance Show OutputFile where
    show (OutputFile s) = " --fo " ++ s ++ " "

-- | Tipo de archivo
data FileType = PNG | TIFF | CSV
    deriving(Read, Show, Eq)

-- | Tipo de función objetivo.
data OFType = Max | Min
    deriving(Read, Eq)

-- | Instancia de Show para OFType.
instance Show OFType where
    show Max = " --tf MAX "
    show Min = " --tf MIN "

-- | Cantidad de Clusters iniciales.
data Clusters = K Int
    deriving(Eq)

-- | Instancia de Show para Clusters.
instance Show Clusters where
    show (K k) = " --k " ++ (show k) ++ " "

-- | Repeticiones sin mejora.
data Repetitions = Reps Int
    deriving (Read,Eq)

-- | Instancia de Show para Repetitions.
instance Show Repetitions where
    show (Reps r) = " --reps " ++ (show r) ++ " "

-- | Cantidad de iteraciones
data Iterations = Iter Int
    deriving (Read, Eq)

-- | Instancia de Show para Iterations.
instance Show Iterations where
    show (Iter r) = " --reps " ++ (show r) ++ " "

-- | Vector de tamaño máximo.
data Mx = Mx (Float, Float, Float)
    deriving(Read, Eq)

-- | Instancia de Show para Mx.
instance Show Mx where
    show (Mx (a, b, c)) = " --mx " ++ (show a) ++
                          "," ++ (show b) ++
                          "," ++ (show c) ++
                          " "

-- | Vector de tamaño máximo.
data Mn = Mn (Float, Float, Float)
    deriving(Read, Eq)

-- | Instancia de Show para Mx.
instance Show Mn where
    show (Mn (a, b, c)) = " --mn " ++ (show a) ++
                          "," ++ (show b) ++
                          "," ++ (show c) ++
                          " "

{-
 - Aleatorios. 
 -}

-- | Cantidad de individuos o partículas.
data Population = I Int
    deriving(Eq)

-- | Instancia de Show para Population.
instance Show Population where
    show (I i) = " --i " ++ (show i) ++ " "

-- | Cantidad de individuos o partículas.
data PopulationGA = IGA Int Int
    deriving(Eq)

-- | Instancia de Show para PopulationGA.
instance Show PopulationGA where
    show (IGA i t) = " --i " ++ (show i) ++
                     " --tt " ++ (show t) ++
                     " "

-- | Cantidad de individuos o partículas.
data PopulationAnt = IAnt Int
    deriving(Eq)

-- | Instancia de Show para PopulationGA.
instance Show PopulationAnt where
    show (IAnt i) = " --i " ++ (show i) ++ " "

-- | Cantidad de individuos o partículas.
data PopulationBee = IBee Int Int Int Int Int
    deriving(Eq)

-- | Instancia de Show para PopulationBee.
instance Show PopulationBee where
    show (IBee i m e eb ob) = " --i " ++ (show i) ++
                              " --m " ++ (show m) ++
                              " --e " ++ (show e) ++
                              " --eb " ++ (show eb) ++
                              " --ob " ++ (show ob) ++
                              " "

-- | Velocidad de las partículas de PSO.
data Velocity = Velocity  { c1 :: Float
                          , c2 :: Float
                          , w  :: Float
                          }
    deriving(Eq)

-- | Instancia de Show para Velocity.
instance Show Velocity where
    show (Velocity a b c) = " --c1 " ++ (show a) ++
                            " --c2 " ++ (show b) ++
                            " --W " ++ (show c) ++
                            " "

-- | Pesos de la función objetivo del PSO y DE.
data Weight = Weight { w1 :: Float
                     , w2 :: Float
                     , w3 :: Float
                     }
    deriving(Eq)

-- | Instancia de Show para Velocity.
instance Show Weight where
    show (Weight a b c) = " --w1 " ++ (show a) ++
                          " --w2 " ++ (show b) ++
                          " --w3 " ++ (show c) ++
                          " "

-- | Velocidad máxima del algoritmo PSO.
data VMax = VMax Float
    deriving(Eq)

-- | Instancia de Show para VMax.
instance Show VMax where
    show (VMax v) = " --vmx " ++ (show v) ++ " "

-- | Probabilidad de mutación.
data Pm = Pm Float
    deriving(Eq)

-- | Instancia de Show para Pm.
instance Show Pm where
    show (Pm f) = " --pm " ++ (show f) ++ " "

-- | Probabilidad de cruce.
data Pc = Pc Float
    deriving(Eq)

-- | Instancia de Show para Pc.
instance Show Pc where
    show (Pc f) = " --pc " ++ (show f) ++ " "

-- | Escalado del vector para el algoritmo DE.
data Scale = Scale Float
    deriving(Eq)

-- | Instancia de Show para Scale.
instance Show Scale where
    show (Scale f) = " --f " ++ (show f) ++ " "

-- | Genera listas de tipo a.
data ListGen a = LG (a, a, a) -- (Inicio, Fin, Paso)
    deriving (Read, Eq, Show)

-- Genera listas a partir de un generador.
listGen :: (Enum a, Num a) => (ListGen a) -> [a]
listGen (LG (a, b, c)) = [a,(a + c)..b]

-- | Opciones de los algoritmos.
data AlgOpt = GAOpt (Repetitions,     -- * Repeticiones sin mejora.
                     ListGen Int,     -- * Población.
                     ListGen Int,     -- * Tamaño de torneo.
                     ListGen Float,   -- * Probabilidad de cruce.
                     ListGen Float)   -- * Probabilidad de mutación.
            | PSOOpt (Repetitions,    -- * Repeticiones sin mejora.
                      ListGen Int,    -- * Población.
                      ListGen Float,  -- * Peso inercial.
                      ListGen Float,  -- * Peso cognitivo.
                      ListGen Float,  -- * Peso social.
                      ListGen Float)  -- * Velocidad máxima.
            | WPSOOpt (Repetitions,   -- * Repeticiones sin mejora.
                       ListGen Int,   -- * Población.
                       ListGen Float, -- * W1.
                       ListGen Float, -- * W2.
                       ListGen Float, -- * w3.
                       ListGen Float, -- * Peso inercial.
                       ListGen Float, -- * Peso cognitivo.
                       ListGen Float, -- * Peso social.
                       ListGen Float) -- * Velocidad máxima.
            | BeeOpt (Repetitions,    -- * Repeticiones sin mejora.
                      ListGen Int,    -- * Población.
                      ListGen Int,    -- * Parches.
                      ListGen Int,    -- * Parches élite.
                      ListGen Int,    -- * Abejas a parches élite.
                      ListGen Int)    -- * Abejas a parches no élite.
    deriving(Show, Eq, Read)

-- | Tipo de datos de los archivos.
type Files = (String, InputFile, OFType, Int, Mn, Mx)

-- | Opciones.
type Options = ([Files], [(String, AlgOpt)])

-- | Dada una lista de opciones de algoritmos, genera varios algoritmos.
genPrograms :: Options    -- * Opciones del los algoritmos.
            -> [Program]  -- * Algoritmos.
genPrograms (f, xs) = progs
    where algopt = concatMap aux0 $ DL.foldl' (genAlgorithm) [] xs
          aux0   = (\(s, xs) -> snd $ DL.foldl' (aux1 s) (0, []) xs )
          aux1   = (\x (i , y) z -> (i + 1, ((x ++ (show i) ++ ".png", z):y)))
          progs  = [ (Program p fi (OutputFile fo) ot (K k) mn mx alg) |
                     (p, fi, ot, k, mn, mx) <- f,
                     (fo, alg) <- algopt
                   ]
          

-- | Operador binario de genAlgorithms.
genAlgorithm :: [(String, [Algorithm])] -- * Lista de algoritmos.
             -> (String, AlgOpt)        -- * Opciones de los algoritmos.
             -> [(String, [Algorithm])] -- * Lista de algoritmos resultantes.
genAlgorithm xs (s, a) = (x:xs)
    where x = (s, genAlgorithm' a)

-- | Genera una lista de Maybe Algorithm
genAlgorithm' :: AlgOpt -> [Algorithm]
genAlgorithm' (GAOpt (r, p, t, pc, pm)) =
    [ (GA r (IGA p' t') (Pc pc') (Pm pm')) |
      p'  <- listGen p,
      t'  <- listGen t,
      pc' <- listGen pc,
      pm' <- listGen pm,
      p' > t'
    ]
genAlgorithm' (PSOOpt (r, p, w, c1, c2, vmx)) =
    [ (PSO r (I p') (Velocity c1' c2' w') (VMax vmx')) |
      p'   <- listGen p,
      w'   <- listGen w,
      c1'  <- listGen c1,
      c2'  <- listGen c2,
      vmx' <- listGen vmx,
      ( ((c1' + c2') * 0.5) - 1.0 < w' )
    ]
genAlgorithm' (WPSOOpt (r, p, w1, w2, w3, w, c1, c2, vmx)) =
    [ (WPSO r (I p') (Velocity c1' c2' w') (VMax vmx') (Weight w1' w2' w3')) |
      p'   <- listGen p,
      w1'  <- listGen w1,
      w2'  <- listGen w2,
      w3'  <- listGen w3,
      w'   <- listGen w,
      c1'  <- listGen c1,
      c2'  <- listGen c2,
      vmx' <- listGen vmx,
      ( ((c1' + c2') * 0.5) - 1.0 < w' ) && ( ((w1' + w2' + w3') - 1.0) < 0.1 )
    ]
genAlgorithm' (BeeOpt (r, p, m, e, eb, ob)) =
    [ (Bee r (IBee p' m' e' eb' ob')) |
      p'  <- listGen p,
      m'  <- listGen m,
      e'  <- listGen e,
      eb' <- listGen eb,
      ob' <- listGen ob,
      (e' < m' - 1) && (ob' < m' - e')
    ]
