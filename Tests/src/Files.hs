{-# LANGUAGE BangPatterns #-}
module Files (
    FileHandles(..),
    runTests
) where

import Pruebas
import Results
import System.IO
import Data.Array
import Control.Monad(when, mapM_)
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Posix.Signals
import qualified Data.List as DL
import qualified System.Process as SP
import qualified System.Directory as SD
import qualified Data.ByteString.Char8 as BS

-- | Descriptores de los archivos.
data FileHandles = FH { hlog :: Handle
                      , hinfo :: Handle
                      , hresult :: Connection
                      }

-- | Si la corrida ha sido terminada o no.
data RunState = FINISHED | UNFINISHED
    deriving(Eq, Show, Read)

-- | Linea de la corrida.
type Log = (RunState, String, Int)

-- | Estado actual de las pruebas.
type CurrentState = (FileHandles, Int)

-- | Signal Handler del cierre de archivos.
endTester :: IConnection conn
          => Handle -- * Handle del log.
          -> Handle -- * Handle del .info.
          -> conn -- * Handle del .result.
          -> IO ()
endTester lh li lr = do
    putStrLn "Finalizando..."
    hClose lh
    hClose li
    disconnect lr

-- | Inicializa el programa de pruebas.
getState :: FilePath -- * Archivo de log.
         -> String -- * Nombre de la prueba.
         -> FilePath -- * Opciones de corrida.
         -> IO CurrentState -- * Estado actual del programa.
getState l n fsource = do
    let finfo = (n ++ ".info")
    let fresult = (n ++ ".db")
    bh <- SD.doesFileExist l
    case bh of
        True -> do
            putStrLn "Leyendo Log..."
            logs <- BS.readFile l
            let logs' = reverse $ map (\x -> read x :: Log) $ lines $ BS.unpack logs
            let last = head $ filter (\(u, s, _) -> finfo == s) logs'
            case last of
                (FINISHED, _, _) -> do
                    putStrLn $ "No hay corridas pendientes con el nombre: " ++ n
                    putStrLn $ "Generando nueva corrida..."
                    -- Generar un nuevo log, archivo .info y .result y ejecutar.
                    os <- BS.readFile fsource
                    let os' = (read (BS.unpack os) :: [Options])
                    let cmds = BS.pack $ unlines $ concat $ map (genPrograms) os'
                    BS.writeFile finfo cmds
                    lh <- openFile l AppendMode
                    li <- openFile finfo ReadMode
                    lr <- connectDB fresult
                    installHandler sigINT (Catch $ endTester lh li lr) Nothing
                    hPutStrLn lh $ show $ (UNFINISHED, finfo, 0)
                    return $ ((FH lh li lr), 0)
                (UNFINISHED, _, i) -> do
                    putStrLn $ "Existe una corrida no terminada."
                    putStrLn $ "Si cree que esto no es así borre el archivo: " ++ l
                    putStrLn $ "Se continuará desde la línea " ++ (show (i + 1)) ++ " de " ++ finfo ++ "..."
                    -- Continuar ejecución del log.
                    lh <- openFile l AppendMode
                    li <- openFile finfo ReadMode
                    lr <- connectDB fresult
                    installHandler sigINT (Catch $ endTester lh li lr) Nothing
                    return $ ((FH lh li lr), i)
        False -> do
            putStrLn $ "Generando log y nueva corrida..."
            -- Generar un nuevo log, archivo .info y .result y ejecutar.
            os <- BS.readFile fsource
            let os' = (read (BS.unpack os) :: [Options])
            let cmds = BS.pack $ unlines $ concat $ map (genPrograms) os'
            BS.writeFile finfo cmds
            lh <- openFile l AppendMode
            li <- openFile finfo ReadWriteMode
            lr <- connectDB fresult
            installHandler sigINT (Catch $ endTester lh li lr) Nothing
            hPutStrLn lh $ show $ (UNFINISHED, finfo, 0)
            return $ ((FH lh li lr), 0)

-- | Genera y corre las pruebas.
runTests l n fsource = do
    (hs, i) <- getState l n fsource
    !cmds <- BS.hGetContents $ hinfo hs
    let cmds' = lines $ BS.unpack cmds
    let cmds'' = listArray (0, (length cmds' - 1)) cmds'
    let top = 1 + (snd $ bounds cmds'')
    run' l n hs cmds'' (i , top)
    putStrLn "Liberando recursos..."
    hClose $ hlog hs
    hClose $ hinfo hs
    disconnect $ hresult hs
    putStrLn "Finalizado =D"

-- | Ejecuta los programas.
run' :: FilePath
     -> String
     -> FileHandles
     -> Array Int String
     -> (Int, Int)
     -> IO ()
run' l n hs cmds (i, f) = do
    case (i == f) of
        True -> do
            hPutStrLn (hlog hs) $ show $ (FINISHED, (n ++ ".info"), i)
            return ()
        False -> do
            let (e:o) = words $ cmds ! i
            s <- SP.readProcess e o ""
            let s' = case parseResult s of
                        Left c -> "Hubo un error al recolectar los datos."
                        Right r -> r
            putStrLn $ unwords (e:o)
            insert (hresult hs) (prepareResults (e:o) s')
            hPutStrLn (hlog hs) $ show $ (UNFINISHED, (n ++ ".info"), i + 1)
            run' l n hs cmds (i + 1, f)

data AlgDB = KmeansDB [SqlValue] | GeneticDB [SqlValue] [SqlValue]

-- | Conecta con la base de datos y crea las tablas si no existen.
connectDB :: FilePath       -- ^ Archivo de la base de datos.
          -> IO Connection  -- ^ Conexión con la base de datos.
connectDB fp = do
    dbh <- connectSqlite3 fp
    prepDB dbh
    return dbh

-- | Prepara la base de datos.
prepDB :: IConnection conn
       => conn
       -> IO ()
prepDB dbh = do
    tables <- getTables dbh
    when (not ("geneticop" `elem` tables)) $
        do run dbh gapDB []
           return ()
    when (not ("genetico" `elem` tables)) $
        do run dbh gaDB []
           return ()
    when (not ("kmeans" `elem` tables)) $
        do run dbh kmeansDB []
           return ()
    commit dbh
    putStrLn "Tablas Creadas."

-- | SQL genético.

-- | Comando SQL para crear una tabla de parámetros del genético.
gapDB :: String
gapDB = "CREATE TABLE geneticop (\
        \ gap_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
        \ gap_i         INTEGER NOT NULL,\
        \ gap_ts        INTEGER NOT NULL,\
        \ gap_cr        REAL NOT NULL,\
        \ gap_mr        REAL NOT NULL)"

-- | Comando SQL para crear una tabla de resultados del genético.
gaDB :: String
gaDB = "CREATE TABLE genetico (\
       \ ga_id    INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
       \ ga_ki    INTEGER NOT NULL,\
       \ ga_kf    INTEGER NOT NULL,\
       \ ga_db    REAL NOT NULL,\
       \ ga_eval  INTEGER NOT NULL,\
       \ ga_turi  REAL NOT NULL,\
       \ ga_time  REAL NOT NULL,\
       \ ga_type  INTEGER NULL,\
       \ FOREIGN KEY(ga_type) REFERENCES geneticop(gap_id))"

kmeansDB :: String
kmeansDB = "CREATE TABLE kmeans (\
           \ kmeans_id    INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
           \ kmeans_ki    INTEGER NOT NULL,\
           \ kmeans_kf    INTEGER NOT NULL,\
           \ kmeans_db    REAL NOT NULL,\
           \ kmeans_eval  INTEGER NOT NULL,\
           \ kmeans_turi  REAL NOT NULL,\
           \ kmeans_time  REAL NOT NULL)"

-- | Comando para encontrar el ID de un conjunto de parámetros.
selectParamGA :: String
selectParamGA = "SELECT gap_id\
                \ FROM geneticop WHERE\
                \ gap_i = ?\
                \ AND gap_ts = ?\
                \ AND gap_cr = ?\
                \ AND gap_mr = ?"

-- | Comando para insertar un conjunto de parámetros.
insertParamGA :: String
insertParamGA = "INSERT INTO geneticop \
                \ (gap_i, gap_cr, gap_mr, gap_ts)\
                \ VALUES (?, ?, ?, ?)"
-- i ts cr mr
-- i cr mr t

-- | Comando para insertar los resultados.
insertResultGA :: String
insertResultGA = "INSERT INTO genetico \
                 \ (ga_ki, ga_kf, ga_db, ga_eval, ga_turi, ga_time, ga_type)\
                 \ VALUES (?, ?, ?, ?, ?, ?, ?)"

insertResultKmeans :: String
insertResultKmeans = "INSERT INTO kmeans \
                     \ (kmeans_ki, kmeans_kf, kmeans_db, kmeans_eval, kmeans_turi, kmeans_time)\
                     \ VALUES (?, ?, ?, ?, ?, ?)"

prepareResults :: [String] -> String -> AlgDB
prepareResults l r = res
    where l'  = snd $ DL.break (== "-I") l
          r'  = words r
          res = case l' of
                    [] -> KmeansDB (genResultSQL r')
                    xs -> GeneticDB (genResultSQL r') (genParamSQL $ extractParams l')

genResultSQL (a:b:c:d:e:f:_) =
    [ toSql (read a :: Int)
    , toSql (read b :: Int)
    , toSql (read c :: Double)
    , toSql (read d :: Int)
    , toSql (read e :: Double)
    , toSql (read f :: Double)
    ]

genParamSQL (a:b:c:d:_) =
    [ toSql (read a :: Int)
    , toSql (read b :: Double)
    , toSql (read c :: Double)
    , toSql (read d :: Int)
    ]

extractParams xs  = filter (\x -> x /= "-I" && x /= "-c" && x /= "-m" && x /= "-t") xs


insert :: IConnection conn
       => conn
       -> AlgDB
       -> IO ()
insert dbh (KmeansDB xs) = do
    run dbh insertResultKmeans xs
    commit dbh
insert dbh (GeneticDB xs ys) = do
    stmt <- prepare dbh selectParamGA
    execute stmt ys
    mRes <- fetchRow stmt
    case mRes of
        Nothing    -> do
            run dbh insertParamGA ys
            execute stmt ys
            (Just [res]) <- fetchRow stmt
            run dbh insertResultGA (xs ++ [res])
            commit dbh
        (Just x) -> do
            run dbh insertResultGA (xs ++ x)
            commit dbh


