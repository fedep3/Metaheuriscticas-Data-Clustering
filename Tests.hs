module Tests(
    -- * Utilidades
    Results(..),
    runTests,
    runTestsAnova,
    -- * Generadores.
    genKmeans,
    genGA,
    genPSO,
    genWPSO,
    genDE,
    genSDE,
    genAnt,
    genBee,
    -- * No aleatorios.
    Program(..),
    Algorithm(..),
    InputFile(..),
    OutputFile(..),
    FileType(..),
    OFType(..),
    Clusters(..),
    Repetitions(..),
    Iterations(..),
    -- * Aleatorios.
    Population(..),
    Velocity(..),
    Weight(..)
) where

import Test.QuickCheck
import Data.List
import Control.Monad
import System.Process

-- | Ejecuta el programa y devuelve la salida del mismo.
run :: Program          -- * Programa a ejecutar
    -> IO String        -- * Salida de la ejecución del programa.
run p = do
    let (e:a) = words $ show p
    readProcess e a ""

-- | Resultado de cada prueba.
data Results = Results { prog   :: String
                       , file   :: String
                       , stdout :: [(String, String)]
                       }

-- | Corre las pruebas de acuerdo a una acción monádica generadora.
runTests :: String         -- * Nombre del ejecutable.
         -> String         -- * Nombre del archivo de entrada.
         -> FileType       -- * Tipo del archivo de entrada.
         -> OFType         -- * Tipo de función objetivo.
         -> Int            -- * Cantidad de Clusters.
         -> IO [Algorithm] -- * Acción monádica.
         -> String         -- * Nombre del algoritmo.
         -> Int            -- * Cantidad de pruebas (múltiplos de 10).
         -> IO Results     -- * Lista de resultados.
runTests p i ft f k m n size = do
    programs  <- genPrograms p i ft f k m n (1, size) []
    let hAnova = (headerAnova . algorithm) $ head programs
    let lanova = (hAnova:(map (getAnova . algorithm) programs))
    lstdout   <- mapM run programs
    let lstdout' = zip lanova ("":(map filterStdout lstdout))
    let l      = ((head lstdout') : (map helper $ tail lstdout'))
    return (Results n i l)

-- | Corre las pruebas de acuerdo a una acción monádica generadora.
runTestsAnova :: String         -- * Nombre del ejecutable.
              -> String         -- * Nombre del archivo de entrada.
              -> FileType       -- * Tipo del archivo de entrada.
              -> OFType         -- * Tipo de función objetivo.
              -> Int            -- * Cantidad de Clusters.
              -> IO [Algorithm] -- * Acción monádica.
              -> String         -- * Nombre del algoritmo.
              -> Int            -- * Cantidad de pruebas (múltiplos de 10).
              -> IO ()
runTestsAnova p i ft f k m n size = do
    r <- runTests p i ft f k m n size
    putStrLn $ unlines $ map fst $ stdout r

filterStdout :: String -> String
filterStdout = unlines . filter ((/=) '-' . head) . lines

helper :: (String, String) -> (String, String)
helper (xs, ys) = ((reverse ss) ++ "\t" ++ xs, ys)
    where (_, (s:ss)) = foldl' (helperBinary ',') ([], "") ys

helperBinary :: Char -> ([String], String) -> Char -> ([String], String)
helperBinary c (x, w) y =
    if (c == y) then ((reverse w):x, [])
                else (x,y:w)
                          

-- | Genera el header para generar la tabla ANOVA.
headerAnova :: Algorithm -> String
headerAnova (Kmeans _)           = ""
headerAnova (GA _ _ _ _)         = "ofdb\ti\ttt\tpc\tpm"
headerAnova (PSO _ _ _ _ _ _)    = "ofdb\ti\tc1\tc2\tw\tvmx"
headerAnova (WPSO _ _ _ _ _ _ _) = "ofdb\ti\tc1\tc2\tw\tvmx\tw1\tw2\tw3"
headerAnova (DE _ _ _ _ _)       = "ofdb\ti\tw1\tw2\tw3"
headerAnova (SDE _ _ _ _ _ _ _)  = "ofdb\ti\tw1\tw2\tw3\tf\tpc"
headerAnova (Ant _ _ _)          = "ofdb\ti\talpha"
headerAnova (Bee _ _)            = "ofdb\ti\tm\te\teb\tob"

-- | Genera una línea apta para generar una tabla ANOVA.
getAnova :: Algorithm   -- * Algoritmo.
         -> String      -- * String con los valores relevantes.
getAnova (Kmeans _) =
    ""
getAnova (GA _ (IGA i t) (Pc pc) (Pm pm)) =
    (show i) ++ "\t" ++ (show t) ++ "\t" ++ (show pc) ++ "\t" ++
    (show pm)
getAnova (PSO _ (I i) _ _ (Velocity c1 c2 w) (VMax vmx)) =
    (show i) ++ "\t" ++ (show c1) ++ "\t" ++ (show c2) ++ "\t" ++
    (show w) ++ "\t" ++ (show vmx)
getAnova (WPSO _ (I i) _ _ (Velocity c1 c2 w) (VMax vmx) (Weight w1 w2 w3)) =
    (show i) ++ "\t" ++ (show c1) ++ "\t" ++ (show c2) ++ "\t" ++
    (show w) ++ "\t" ++ (show vmx) ++ "\t" ++ (show w1) ++ "\t" ++
    (show w2) ++ "\t" ++ (show w3)
getAnova (DE _ (I i) _ _ (Weight w1 w2 w3)) =
    (show i) ++ "\t" ++ (show w1) ++ "\t" ++ (show w2) ++ "\t" ++
    (show w3)
getAnova (SDE _ (I i) _ _ (Weight w1 w2 w3) (Scale s) (Pc pc)) =
    (show i) ++ "\t" ++ (show w1) ++ "\t" ++ (show w2) ++ "\t" ++
    (show w3) ++ "\t" ++ (show s) ++ "\t" ++ (show pc)
getAnova (Ant _ (IAnt i) a) =
    (show i) ++ "\t" ++ (show a)
getAnova (Bee _ (IBee i m e eb ob)) =
    (show i) ++ "\t" ++ (show m) ++ "\t" ++ (show e) ++ "\t" ++
    (show eb) ++ "\t" ++ (show ob)

{-
 - No aleatorios. 
 -}

-- | Tipo para representar una llamada al programa.
data Program = Program { name       :: String
                       , inputFile  :: InputFile
                       , outputFile :: OutputFile
                       , oftype     :: OFType
                       , clusters   :: Clusters
                       , algorithm  :: Algorithm
                       }
    deriving(Eq)

-- | Instancia de Show para Program.
instance Show Program where
    show (Program p i o t k a) = "./" ++ p ++ " -d " ++
                                 (show i) ++ (show o) ++
                                 (show t) ++ (show k) ++
                                 (show a)

-- | Genera un programa dado un algoritmo.
genProgram :: String    -- * Nombre del ejecutable.
           -> String    -- * Nombre del archivo de entrada.
           -> FileType  -- * Tipo del archivo de entrada.
           -> String    -- * Nombre del archivo de salida.
           -> OFType    -- * Tipo de función objetivo.
           -> Int       -- * Cantidad de Clusters.
           -> Algorithm -- * Algoritmo a ejecutar.
           -> Program   -- * Línea de comándos resultante.
genProgram p i ft o f k a =
    if (k > 0) then (Program p
                             (InputFile i ft)
                             (OutputFile o)
                             f
                             (K k)
                             a)
                else error "K debe ser mayor de 0"

-- | Genera programas dada una acción monádica.
genPrograms :: String       -- * Nombre del ejecutable.
            -> String       -- * Nombre del archivo de entrada.
            -> FileType     -- * Tipo del archivo de entrada.
            -> OFType       -- * Tipo de función objetivo.
            -> Int          -- * Cantidad de Clusters.
            -> IO [Algorithm]-- * Acción monádica.
            -> String       -- * Nombre del algoritmo.
            -> (Int, Int)   -- * Inicio y fin de la numeración.
            -> [Program]    -- * Acumulador.
            -> IO [Program] -- * Lista de programas.
genPrograms p i ft f k m n (ini, end) a = do
    case (ini >= end) of
        True  -> return $ reverse a
        False -> do
            l <- m
            let ns = genResultName n i ini
            let z  = zip ns l
            let a' = foldl' (binary p i ft f k) [] z
            genPrograms p i ft f k m n (ini + 10, end) (a' ++ a)

-- | Operador binario auxiliar para genPrograms
binary p i ft f k l (a, b) = ((genProgram p i ft a f k b): l)

-- | Genera los nombres de las salidas del algoritmo.
genResultName :: String   -- * Nombre del algoritmo.
              -> String   -- * Nombre del archivo.
              -> Int      -- * Inicio de la numeración.
              -> [String] -- * Lista de nombres.
genResultName alg file ini = zipWith fun l [ini..]
    where fun = (\a b -> a ++ (show b) ++ ".png")
          l   = (take 10 $ repeat (alg ++ "_" ++ f ++ "_"))
          f   = reverse $ snd $ foldl' (helperBinary '/') ([], "") file

-- | Tipo de dato que representa a un algoritmo.
data Algorithm = Kmeans Repetitions
               | GA Repetitions PopulationGA Pc Pm
               | PSO Repetitions Population Mx Mn Velocity VMax
               | WPSO Repetitions Population Mx Mn Velocity VMax Weight
               | DE Iterations Population Mx Mn Weight
               | SDE Iterations Population Mx Mn Weight Scale Pc
               | Ant Iterations PopulationAnt Float
               | Bee Repetitions PopulationBee
    deriving(Eq)

-- | Genera 10 casos de prueba idénticos para Kmeans.
genKmeans :: Int            -- * Repeticiones sin mejora.
          -> IO [Algorithm] -- * Corridas generadas.
genKmeans r = return $ take 10 $ repeat (Kmeans (Reps r))

-- | Genera 10 casos de prueba aleatorios para GA.
genGA :: Int            -- * Repeticiones sin mejora.
      -> IO [Algorithm] -- * Corridas generadas.
genGA r = do
    p  <- sample' (arbitrary :: Gen (PopulationGA))
    pc <- sample' (arbitrary :: Gen (Pc))
    pm <- sample' (arbitrary :: Gen (Pm))
    let r' = take 10 $ repeat (Reps r)
    let z  = zip4 r' p pc pm
    return $ foldl' (\l (a, b, c, d) -> ((GA a b c d):l)) [] z

-- | Genera 10 casos de prueba aleatorios para PSO.
genPSO :: Int                   -- * Repeticiones sin mejora.
       -> (Float, Float, Float) -- * Vector máximo.
       -> (Float, Float, Float) -- * Vector mínimo.
       -> IO [Algorithm]        -- * Corridas generadas.
genPSO r mx mn = do
    let r'  = take 10 $ repeat (Reps r)
    let mx' = take 10 $ repeat (Mx mx)
    let mn' = take 10 $ repeat (Mn mn)
    p  <- sample' (arbitrary :: Gen (Population))
    v  <- sample' (arbitrary :: Gen (Velocity))
    vx <- sample' (arbitrary :: Gen (VMax))
    let z   = zip6 r' p mx' mn' v vx
    return $ foldl' (\l (a,b,c,d,e,f) -> ((PSO a b c d e f):l)) [] z

-- | Genera 10 casos de prueba aleatorios para WPSO.
genWPSO :: Int                   -- * Repeticiones sin mejora.
       -> (Float, Float, Float) -- * Vector máximo.
       -> (Float, Float, Float) -- * Vector mínimo.
       -> IO [Algorithm]        -- * Corridas generadas.
genWPSO r mx mn = do
    let r'  = take 10 $ repeat (Reps r)
    let mx' = take 10 $ repeat (Mx mx)
    let mn' = take 10 $ repeat (Mn mn)
    p  <- sample' (arbitrary :: Gen (Population))
    v  <- sample' (arbitrary :: Gen (Velocity))
    vx <- sample' (arbitrary :: Gen (VMax))
    w  <- sample' (arbitrary :: Gen (Weight))
    let z   = zip7 r' p mx' mn' v vx w
    return $ foldl' (\l (a,b,c,d,e,f,g) -> ((WPSO a b c d e f g):l)) [] z

-- | Genera 10 casos de prueba aleatorios para DE.
genDE :: Int            -- * Iteraciones.
      -> (Float, Float, Float) -- * Vector máximo.
      -> (Float, Float, Float) -- * Vector mínimo.
      -> IO [Algorithm]        -- * Corridas generadas.
genDE r mx mn = do
    p <- sample' (arbitrary :: Gen (Population))
    w <- sample' (arbitrary :: Gen (Weight))
    let mx' = take 10 $ repeat (Mx mx)
    let mn' = take 10 $ repeat (Mn mn)
    let r' = take 10 $ repeat (Iter r)
    let z  = zip5 r' p mx' mn' w
    return $ foldl' (\l (a,b,c,d,e) -> ((DE a b c d e):l)) [] z

-- | Genera 10 casos de prueba aleatorios para SDE.
genSDE :: Int            -- * Iteraciones.
       -> (Float, Float, Float) -- * Vector máximo.
       -> (Float, Float, Float) -- * Vector mínimo.
       -> IO [Algorithm]        -- * Corridas generadas.
genSDE r mx mn = do
    p  <- sample' (arbitrary :: Gen (Population))
    w  <- sample' (arbitrary :: Gen (Weight))
    pc <- sample' (arbitrary :: Gen (Pc))
    s  <- sample' (arbitrary :: Gen (Scale))
    let mx' = take 10 $ repeat (Mx mx)
    let mn' = take 10 $ repeat (Mn mn)
    let r' = take 10 $ repeat (Iter r)
    let z  = zip7 r' p mx' mn' w s pc
    return $ foldl' (\l (a,b,c,d,e,f,g) -> ((SDE a b c d e f g):l)) [] z

-- | Genera 10 casos de prueba aleatorios para Ant.
genAnt :: Int            -- * Iteraciones.
       -> IO [Algorithm] -- * Corridas generadas.
genAnt r = do
    p <- sample' (arbitrary :: Gen (PopulationAnt))
    a <- sample' ( choose (0.0, 441.67) )
    let r' = take 10 $ repeat (Iter r)
    let z  = zip3 r' p a
    return $ foldl' (\l (a,b,c) -> ((Ant a b c):l)) [] z

-- | Genera 10 casos de prueba idénticos para Bee.
genBee :: Int            -- * Repeticiones sin mejora.
       -> IO [Algorithm] -- * Corridas generadas.
genBee r = do
    p <- sample' (arbitrary :: Gen (PopulationBee))
    let r' = take 10 $ repeat (Reps r)
    let z  = zip r' p
    return $ foldl' (\l (a,b) -> ((Bee a b):l)) [] z

-- | Instancia de Show para Algorithm.
instance Show Algorithm where
    show (Kmeans r)              = " --a Kmeans " ++ (show r)
    show (GA r i pc pm)          = " --a GA " ++
                                   (show r) ++ (show i) ++
                                   (show pc) ++ (show pm)
    show (PSO r i mx mn v vx)    = " --a PSO " ++
                                   (show r)  ++ (show i) ++
                                   (show mx) ++ (show mn) ++
                                   (show v)  ++ (show vx)
    show (WPSO r i mx mn v vx w) = " --a PSO" ++
                                   (show w) ++ (show r) ++
                                   (show i) ++ (show mx) ++
                                   (show mn) ++ (show v) ++
                                   (show vx)
    show (DE r i mx mn w)        = " --a DE " ++ (show r) ++
                                   (show i) ++ (show mx) ++
                                   (show mn) ++ (show w)
    show (SDE r i mx mn w s pc)  = " --a DE " ++ (show r) ++
                                   (show i) ++ (show mx) ++
                                   (show mn) ++ (show w) ++
                                   (show s) ++ (show pc)
    show (Ant r i a)             = " --a Ant " ++ (show r) ++
                                   (show i) ++ (show a) ++ " "
    show (Bee r i)               = " --a Bee " ++ (show r) ++
                                   (show i)
    

-- | Archivo de entrada del algoritmo.
data InputFile = InputFile String FileType
    deriving(Eq)

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
    deriving(Show, Eq)

-- | Tipo de función objetivo.
data OFType = Max | Min
    deriving(Eq)

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
    deriving (Eq)

-- | Instancia de Show para Repetitions.
instance Show Repetitions where
    show (Reps r) = " --reps " ++ (show r) ++ " "

-- | Cantidad de iteraciones
data Iterations = Iter Int
    deriving (Eq)

-- | Instancia de Show para Iterations.
instance Show Iterations where
    show (Iter r) = " --reps " ++ (show r) ++ " "

-- | Vector de tamaño máximo.
data Mx = Mx (Float, Float, Float)
    deriving(Eq)

-- | Instancia de Show para Mx.
instance Show Mx where
    show (Mx (a, b, c)) = " --mx " ++ (show a) ++
                          "," ++ (show b) ++
                          "," ++ (show c) ++
                          " "

-- | Vector de tamaño máximo.
data Mn = Mn (Float, Float, Float)
    deriving(Eq)

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

-- | Instancia de Arbitrary para Population.
instance Arbitrary Population where
    arbitrary = do
        i <- choose (20, 30)
        return (I i)

-- | Cantidad de individuos o partículas.
data PopulationGA = IGA Int Int
    deriving(Eq)

-- | Instancia de Show para PopulationGA.
instance Show PopulationGA where
    show (IGA i t) = " --i " ++ (show i) ++
                     " --tt " ++ (show t) ++
                     " "

-- | Instancia de Arbitrary para PopulationGA.
instance Arbitrary PopulationGA where
    arbitrary = do
        i <- choose (20, 30)
        t <- choose (10, 15)
        return (IGA i t)

-- | Cantidad de individuos o partículas.
data PopulationAnt = IAnt Int
    deriving(Eq)

-- | Instancia de Show para PopulationGA.
instance Show PopulationAnt where
    show (IAnt i) = " --i " ++ (show i) ++ " "

-- | Instancia de Arbitrary para PopulationGA.
instance Arbitrary PopulationAnt where
    arbitrary = do
        i <- choose (1, 20)
        return (IAnt i)

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

-- | Instancia de Arbitrary para Population.
instance Arbitrary PopulationBee where
    arbitrary = do
        i  <- choose (20, 30)
        m  <- choose (5, 10)
        e  <- choose (1, m - 1)
        eb <- choose (5, 10)
        ob <- choose (1, m - e)
        return (IBee i m e eb ob)

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

-- | Instancia de Arbitrary para Weight.
instance Arbitrary Velocity where
    arbitrary = do
        a <- choose (0.0, 2.0)
        b <- choose (0.0, 2.0)
        let prop_vel = (\x y z -> ( ( ( ( (x + y) * 0.5) - 1.0) < z )
                                   && (2.0 >= z && z >= 0.0) ))
        c <- suchThat (arbitrary :: Gen Float) (prop_vel a b)
        return (Velocity a b c)

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

-- | Instancia de Arbitrary para Weight.
instance Arbitrary Weight where
    arbitrary = do
        a <- choose (0.0, 1.0)
        b <- choose (0.0, 1.0)
        case (a + b) > 1.0 of
            False -> do
                let c = 1.0 - (a + b)
                return (Weight a b c)
            True  -> do
                let a' = 1.0 - a
                let b' = 1.0 - b
                let c' = 1.0 - (a' + b')
                return (Weight a' b' c')

-- | Velocidad máxima del algoritmo PSO.
data VMax = VMax Float
    deriving(Eq)

-- | Instancia de Show para VMax.
instance Show VMax where
    show (VMax v) = " --vmx " ++ (show v) ++ " "

-- | Instancia de Arbitrary para VMax.
instance Arbitrary VMax where
    arbitrary = do
        n <- choose (0.0, 255.0)
        return (VMax n)

-- | Probabilidad de mutación.
data Pm = Pm Float
    deriving(Eq)

-- | Instancia de Show para Pm.
instance Show Pm where
    show (Pm f) = " --pm " ++ (show f) ++ " "

-- | Instancia de Arbitrary para Pm.
instance Arbitrary Pm where
    arbitrary = do
        n <- choose (0.0, 1.0)
        return (Pm n)

-- | Probabilidad de cruce.
data Pc = Pc Float
    deriving(Eq)

-- | Instancia de Show para Pc.
instance Show Pc where
    show (Pc f) = " --pc " ++ (show f) ++ " "

-- | Instancia de Arbitrary para Pc.
instance Arbitrary Pc where
    arbitrary = do
        n <- choose (0.0, 1.0)
        return (Pc n)

-- | Escalado del vector para el algoritmo DE.
data Scale = Scale Float
    deriving(Eq)

-- | Instancia de Show para Scale.
instance Show Scale where
    show (Scale f) = " --f " ++ (show f) ++ " "

-- | Instancia de Arbitrary para Scale.
instance Arbitrary Scale where
    arbitrary = do
        n <- choose (0.5, 1.0)
        return (Scale n)
