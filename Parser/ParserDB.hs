{-# LANGUAGE BangPatterns #-}
module ParserDB (
    connectDB,
    fillDB,
    AlgType(..)
)where

import Control.Monad(when, mapM_)
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO(readFile)

-- | Conecta con la base de datos y crea las tablas si no existen.
connectDB :: FilePath       -- ^ Archivo de la base de datos.
          -> IO Connection  -- ^ Conexión con la base de datos.
connectDB fp = do
    dbh <- connectSqlite3 fp
    prepDB dbh
    return dbh

-- | Llena la base de datos.
fillDB :: IConnection conn
       => conn
       -> FilePath
       -> AlgType
       -> IO ()
fillDB dbh fp t = do
    s               <- readFile fp
    (s0, s1, s2, a) <- parse dbh t $ lines s
    mapM_ (insertParam dbh s0 s1 s2) a
    commit dbh
    disconnect dbh

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
    when (not ("abejap" `elem` tables)) $
        do run dbh beepDB []
           return ()
    when (not ("abeja" `elem` tables)) $
        do run dbh beeDB []
           return ()
    when (not ("dep" `elem` tables)) $
        do run dbh depDB []
           return ()
    when (not ("de" `elem` tables)) $
        do run dbh deDB []
           return ()
    when (not ("sdep" `elem` tables)) $
        do run dbh sdepDB []
           return ()
    when (not ("sde" `elem` tables)) $
        do run dbh sdeDB []
           return ()
    when (not ("hormigap" `elem` tables)) $
        do run dbh antpDB []
           return ()
    when (not ("hormiga" `elem` tables)) $
        do run dbh antDB []
           return ()
    when (not ("psop" `elem` tables)) $
        do run dbh psopDB []
           return ()
    when (not ("pso" `elem` tables)) $
        do run dbh psoDB []
           return ()
    when (not ("wpsop" `elem` tables)) $
        do run dbh wpsopDB []
           return ()
    when (not ("wpso" `elem` tables)) $
        do run dbh wpsoDB []
           return ()
    commit dbh
    putStrLn "Tablas Creadas."

-- | SQL genético.

-- | Comando SQL para crear una tabla de parámetros del genético.
gapDB :: String
gapDB = "CREATE TABLE geneticop (\
        \ gap_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
        \ gap_i         INTEGER NOT NULL,\
        \ gap_tt        INTEGER NOT NULL,\
        \ gap_pc        REAL NOT NULL,\
        \ gap_pm        REAL NOT NULL)"

-- | Comando SQL para crear una tabla de resultados del genético.
gaDB :: String
gaDB = "CREATE TABLE genetico (\
       \ ga_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
       \ ga_cluster_i INTEGER NOT NULL,\
       \ ga_alg_time  REAL NOT NULL,\
       \ ga_hib_time  REAL NOT NULL,\
       \ ga_cluster_f INTEGER NOT NULL,\
       \ ga_alg_eval  INTEGER NOT NULL,\
       \ ga_alg_fo    REAL NOT NULL,\
       \ ga_alg_db    REAL NOT NULL,\
       \ ga_alg_je    REAL NOT NULL,\
       \ ga_km_eval   INTEGER NOT NULL,\
       \ ga_hib_eval  INTEGER NOT NULL,\
       \ ga_hib_fo    REAL NOT NULL,\
       \ ga_hib_db    REAL NOT NULL,\
       \ ga_hib_je    REAL NOT NULL,\
       \ ga_type      INTEGER NULL,\
       \ FOREIGN KEY(ga_type) REFERENCES geneticop(gap_id))"

-- | Comando para encontrar el ID de un conjunto de parámetros.
selectParamGA :: String
selectParamGA = "SELECT gap_id\
                \ FROM geneticop WHERE\
                \ gap_i = ?\
                \ AND gap_tt = ?\
                \ AND gap_pc = ?\
                \ AND gap_pm = ?"

-- | Comando para insertar un conjunto de parámetros.
insertParamGA :: String
insertParamGA = "INSERT INTO geneticop \
                \ (gap_i, gap_tt, gap_pc, gap_pm)\
                \ VALUES (?, ?, ?, ?)"

-- | Comando para insertar los resultados.
insertResultGA :: String
insertResultGA = "INSERT INTO genetico \
                 \ (ga_cluster_i, ga_alg_time, ga_hib_time, ga_cluster_f,\
                 \ ga_alg_eval, ga_alg_fo, ga_alg_db, ga_alg_je, ga_km_eval,\
                 \ ga_hib_eval, ga_hib_fo, ga_hib_db, ga_hib_je, ga_type)\
                 \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

-- SQL Abeja.

-- | Comando SQL para crear una tabla de parámetros del abeja.
beepDB :: String
beepDB = "CREATE TABLE abejap (\
         \ beep_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
         \ beep_i         INTEGER NOT NULL,\
         \ beep_m         INTEGER NOT NULL,\
         \ beep_e         INTEGER NOT NULL,\
         \ beep_eb        INTEGER NOT NULL,\
         \ beep_ob        INTEGER NOT NULL)"

-- | Comando SQL para crear una tabla de resultados del abeja.
beeDB :: String
beeDB = "CREATE TABLE abeja (\
       \ bee_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
       \ bee_cluster_i INTEGER NOT NULL,\
       \ bee_alg_time  REAL NOT NULL,\
       \ bee_hib_time  REAL NOT NULL,\
       \ bee_cluster_f INTEGER NOT NULL,\
       \ bee_alg_eval  INTEGER NOT NULL,\
       \ bee_alg_fo    REAL NOT NULL,\
       \ bee_alg_db    REAL NOT NULL,\
       \ bee_alg_je    REAL NOT NULL,\
       \ bee_km_eval   INTEGER NOT NULL,\
       \ bee_hib_eval  INTEGER NOT NULL,\
       \ bee_hib_fo    REAL NOT NULL,\
       \ bee_hib_db    REAL NOT NULL,\
       \ bee_hib_je    REAL NOT NULL,\
       \ bee_type      INTEGER NULL,\
       \ FOREIGN KEY(bee_type) REFERENCES abejap(beep_id))"

-- | Comando para encontrar el ID de un conjunto de parámetros.
selectParamBee :: String
selectParamBee = "SELECT beep_id\
                 \ FROM abejap WHERE\
                 \ beep_i = ?\
                 \ AND beep_m = ?\
                 \ AND beep_e = ?\
                 \ AND beep_eb = ?\
                 \ AND beep_ob = ?"

-- | Comando para insertar un conjunto de parámetros.
insertParamBee :: String
insertParamBee = "INSERT INTO abejap \
                 \ (beep_i, beep_m, beep_e, beep_eb, beep_ob)\
                 \ VALUES (?, ?, ?, ?, ?)"

-- | Comando para insertar los resultados.
insertResultBee :: String
insertResultBee = "INSERT INTO abeja \
                  \ (bee_cluster_i, bee_alg_time, bee_hib_time, bee_cluster_f,\
                  \ bee_alg_eval, bee_alg_fo, bee_alg_db, bee_alg_je, bee_km_eval,\
                  \ bee_hib_eval, bee_hib_fo, bee_hib_db, bee_hib_je, bee_type)\
                  \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

-- | Comando SQL para crear una tabla de parámetros del DE.
depDB :: String
depDB = "CREATE TABLE dep (\
        \ dep_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
        \ dep_i         INTEGER NOT NULL,\
        \ dep_w1        REAL NOT NULL,\
        \ dep_w2        REAL NOT NULL,\
        \ dep_w3        REAL NOT NULL)"

-- | Comando SQL para crear una tabla de resultados del DE.
deDB :: String
deDB = "CREATE TABLE de (\
       \ de_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
       \ de_cluster_i INTEGER NOT NULL,\
       \ de_alg_time  REAL NOT NULL,\
       \ de_hib_time  REAL NOT NULL,\
       \ de_cluster_f INTEGER NOT NULL,\
       \ de_alg_eval  INTEGER NOT NULL,\
       \ de_alg_fo    REAL NOT NULL,\
       \ de_alg_db    REAL NOT NULL,\
       \ de_alg_je    REAL NOT NULL,\
       \ de_km_eval   INTEGER NOT NULL,\
       \ de_hib_eval  INTEGER NOT NULL,\
       \ de_hib_fo    REAL NOT NULL,\
       \ de_hib_db    REAL NOT NULL,\
       \ de_hib_je    REAL NOT NULL,\
       \ de_type      INTEGER NULL,\
       \ FOREIGN KEY(de_type) REFERENCES dep(dep_id))"

-- | Comando para encontrar el ID de un conjunto de parámetros.
selectParamDE :: String
selectParamDE = "SELECT dep_id\
                \ FROM dep WHERE\
                \ dep_i = ?\
                \ AND dep_w1 = ?\
                \ AND dep_w2 = ?\
                \ AND dep_w3 = ?"

-- | Comando para insertar un conjunto de parámetros.
insertParamDE :: String
insertParamDE = "INSERT INTO dep \
                \ (dep_i, dep_w1, dep_w2, dep_w3)\
                \ VALUES (?, ?, ?, ?)"

-- | Comando para insertar los resultados.
insertResultDE :: String
insertResultDE = "INSERT INTO de \
                 \ (de_cluster_i, de_alg_time, de_hib_time, de_cluster_f,\
                 \ de_alg_eval, de_alg_fo, de_alg_db, de_alg_je, de_km_eval,\
                 \ de_hib_eval, de_hib_fo, de_hib_db, de_hib_je, de_type)\
                 \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

-- | Comando SQL para crear una tabla de parámetros del SDE.
sdepDB :: String
sdepDB = "CREATE TABLE sdep (\
        \ sdep_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
        \ sdep_i         INTEGER NOT NULL,\
        \ sdep_w1        REAL NOT NULL,\
        \ sdep_w2        REAL NOT NULL,\
        \ sdep_w3        REAL NOT NULL,\
        \ sdep_f         REAL NOT NULL,\
        \ sdep_pc        REAL NOT NULL)"

-- | Comando SQL para crear una tabla de resultados del SDE.
sdeDB :: String
sdeDB = "CREATE TABLE sde (\
       \ sde_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
       \ sde_cluster_i INTEGER NOT NULL,\
       \ sde_alg_time  REAL NOT NULL,\
       \ sde_hib_time  REAL NOT NULL,\
       \ sde_cluster_f INTEGER NOT NULL,\
       \ sde_alg_eval  INTEGER NOT NULL,\
       \ sde_alg_fo    REAL NOT NULL,\
       \ sde_alg_db    REAL NOT NULL,\
       \ sde_alg_je    REAL NOT NULL,\
       \ sde_km_eval   INTEGER NOT NULL,\
       \ sde_hib_eval  INTEGER NOT NULL,\
       \ sde_hib_fo    REAL NOT NULL,\
       \ sde_hib_db    REAL NOT NULL,\
       \ sde_hib_je    REAL NOT NULL,\
       \ sde_type      INTEGER NULL,\
       \ FOREIGN KEY(sde_type) REFERENCES sdep(sdep_id))"

-- | Comando para encontrar el ID de un conjunto de parámetros.
selectParamSDE :: String
selectParamSDE = "SELECT sdep_id\
                 \ FROM sdep WHERE\
                 \ sdep_i = ?\
                 \ AND sdep_w1 = ?\
                 \ AND sdep_w2 = ?\
                 \ AND sdep_w3 = ?\
                 \ AND sdep_f = ?\
                 \ AND sdep_pc = ?"

-- | Comando para insertar un conjunto de parámetros.
insertParamSDE :: String
insertParamSDE = "INSERT INTO sdep \
                 \ (sdep_i, sdep_w1, sdep_w2, sdep_w3, sdep_f, sdep_pc)\
                 \ VALUES (?, ?, ?, ?, ?, ?)"

-- | Comando para insertar los resultados.
insertResultSDE :: String
insertResultSDE = "INSERT INTO sde \
                 \ (sde_cluster_i, sde_alg_time, sde_hib_time, sde_cluster_f,\
                 \ sde_alg_eval, sde_alg_fo, sde_alg_db, sde_alg_je, sde_km_eval,\
                 \ sde_hib_eval, sde_hib_fo, sde_hib_db, sde_hib_je, sde_type)\
                 \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

-- | Comando SQL para crear una tabla de parámetros del hormiga.
antpDB :: String
antpDB = "CREATE TABLE hormigap (\
        \ antp_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
        \ antp_i         INTEGER NOT NULL)"

-- | Comando SQL para crear una tabla de resultados del hormiga.
antDB :: String
antDB = "CREATE TABLE hormiga (\
       \ ant_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
       \ ant_cluster_i INTEGER NOT NULL,\
       \ ant_alg_time  REAL NOT NULL,\
       \ ant_hib_time  REAL NOT NULL,\
       \ ant_cluster_f INTEGER NOT NULL,\
       \ ant_alg_eval  INTEGER NOT NULL,\
       \ ant_alg_fo    REAL NOT NULL,\
       \ ant_alg_db    REAL NOT NULL,\
       \ ant_alg_je    REAL NOT NULL,\
       \ ant_km_eval   INTEGER NOT NULL,\
       \ ant_hib_eval  INTEGER NOT NULL,\
       \ ant_hib_fo    REAL NOT NULL,\
       \ ant_hib_db    REAL NOT NULL,\
       \ ant_hib_je    REAL NOT NULL,\
       \ ant_type      INTEGER NULL,\
       \ FOREIGN KEY(ant_type) REFERENCES hormigap(antp_id))"

-- | Comando para encontrar el ID de un conjunto de parámetros.
selectParamAnt :: String
selectParamAnt = "SELECT antp_id\
                 \ FROM hormigap WHERE\
                 \ antp_i = ?"

-- | Comando para insertar un conjunto de parámetros.
insertParamAnt :: String
insertParamAnt = "INSERT INTO hormigap \
                 \ (antp_i)\
                 \ VALUES (?)"

-- | Comando para insertar los resultados.
insertResultAnt :: String
insertResultAnt = "INSERT INTO hormiga \
                  \ (ant_cluster_i, ant_alg_time, ant_hib_time, ant_cluster_f,\
                  \ ant_alg_eval, ant_alg_fo, ant_alg_db, ant_alg_je, ant_km_eval,\
                  \ ant_hib_eval, ant_hib_fo, ant_hib_db, ant_hib_je, ant_type)\
                  \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

-- | Comando SQL para crear una tabla de parámetros del PSO.
psopDB :: String
psopDB = "CREATE TABLE psop (\
        \ psop_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
        \ psop_i         INTEGER NOT NULL,\
        \ psop_c1        REAL NOT NULL,\
        \ psop_c2        REAL NOT NULL,\
        \ psop_w         REAL NOT NULL,\
        \ psop_vmx       REAL NOT NULL)"

-- | Comando SQL para crear una tabla de resultados del PSO.
psoDB :: String
psoDB = "CREATE TABLE pso (\
       \ pso_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
       \ pso_cluster_i INTEGER NOT NULL,\
       \ pso_alg_time  REAL NOT NULL,\
       \ pso_hib_time  REAL NOT NULL,\
       \ pso_cluster_f INTEGER NOT NULL,\
       \ pso_alg_eval  INTEGER NOT NULL,\
       \ pso_alg_fo    REAL NOT NULL,\
       \ pso_alg_db    REAL NOT NULL,\
       \ pso_alg_je    REAL NOT NULL,\
       \ pso_km_eval   INTEGER NOT NULL,\
       \ pso_hib_eval  INTEGER NOT NULL,\
       \ pso_hib_fo    REAL NOT NULL,\
       \ pso_hib_db    REAL NOT NULL,\
       \ pso_hib_je    REAL NOT NULL,\
       \ pso_type      INTEGER NULL,\
       \ FOREIGN KEY(pso_type) REFERENCES psop(psop_id))"

-- | Comando para encontrar el ID de un conjunto de parámetros.
selectParamPSO :: String
selectParamPSO = "SELECT psop_id\
                 \ FROM psop WHERE\
                 \ psop_i = ?\
                 \ AND psop_c1 = ?\
                 \ AND psop_c2 = ?\
                 \ AND psop_w = ?\
                 \ AND psop_vmx = ?"

-- | Comando para insertar un conjunto de parámetros.
insertParamPSO :: String
insertParamPSO = "INSERT INTO psop \
                 \ (psop_i, psop_c1, psop_c2, psop_w, psop_vmx)\
                 \ VALUES (?, ?, ?, ?, ?)"

-- | Comando para insertar los resultados.
insertResultPSO :: String
insertResultPSO = "INSERT INTO pso \
                 \ (pso_cluster_i, pso_alg_time, pso_hib_time, pso_cluster_f,\
                 \ pso_alg_eval, pso_alg_fo, pso_alg_db, pso_alg_je, pso_km_eval,\
                 \ pso_hib_eval, pso_hib_fo, pso_hib_db, pso_hib_je, pso_type)\
                 \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

-- | Comando SQL para crear una tabla de parámetros del WPSO.
wpsopDB :: String
wpsopDB = "CREATE TABLE wpsop (\
        \ wpsop_id         INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
        \ wpsop_i          INTEGER NOT NULL,\
        \ wpsop_c1         REAL NOT NULL,\
        \ wpsop_c2         REAL NOT NULL,\
        \ wpsop_w          REAL NOT NULL,\
        \ wpsop_vmx        REAL NOT NULL,\
        \ wpsop_w1         REAL NOT NULL,\
        \ wpsop_w2         REAL NOT NULL,\
        \ wpsop_w3         REAL NOT NULL)"

-- | Comando SQL para crear una tabla de resultados del WPSO.
wpsoDB :: String
wpsoDB = "CREATE TABLE wpso (\
       \ wpso_id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
       \ wpso_cluster_i INTEGER NOT NULL,\
       \ wpso_alg_time  REAL NOT NULL,\
       \ wpso_hib_time  REAL NOT NULL,\
       \ wpso_cluster_f INTEGER NOT NULL,\
       \ wpso_alg_eval  INTEGER NOT NULL,\
       \ wpso_alg_fo    REAL NOT NULL,\
       \ wpso_alg_db    REAL NOT NULL,\
       \ wpso_alg_je    REAL NOT NULL,\
       \ wpso_km_eval   INTEGER NOT NULL,\
       \ wpso_hib_eval  INTEGER NOT NULL,\
       \ wpso_hib_fo    REAL NOT NULL,\
       \ wpso_hib_db    REAL NOT NULL,\
       \ wpso_hib_je    REAL NOT NULL,\
       \ wpso_type      INTEGER NULL,\
       \ FOREIGN KEY(wpso_type) REFERENCES wpsop(wpsop_id))"

-- | Comando para encontrar el ID de un conjunto de parámetros.
selectParamWPSO :: String
selectParamWPSO = "SELECT wpsop_id\
                 \ FROM wpsop WHERE\
                 \ wpsop_i = ?\
                 \ AND wpsop_c1 = ?\
                 \ AND wpsop_c2 = ?\
                 \ AND wpsop_w = ?\
                 \ AND wpsop_vmx = ?\
                 \ AND wpsop_w1 = ?\
                 \ AND wpsop_w2 = ?\
                 \ AND wpsop_w3 = ?"

-- | Comando para insertar un conjunto de parámetros.
insertParamWPSO :: String
insertParamWPSO = "INSERT INTO wpsop \
                  \ (wpsop_i, wpsop_c1, wpsop_c2, wpsop_w, wpsop_vmx, wpsop_w1, wpsop_w2, wpsop_w3)\
                  \ VALUES (?, ?, ?, ?, ?, ?, ?, ?)"

-- | Comando para insertar los resultados.
insertResultWPSO :: String
insertResultWPSO = "INSERT INTO wpso \
                   \ (wpso_cluster_i, wpso_alg_time, wpso_hib_time, wpso_cluster_f,\
                   \ wpso_alg_eval, wpso_alg_fo, wpso_alg_db, wpso_alg_je, wpso_km_eval,\
                   \ wpso_hib_eval, wpso_hib_fo, wpso_hib_db, wpso_hib_je, wpso_type)\
                   \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

-- | Tipos de algoritmos.
data AlgType = TGA | TBee | TDE | TSDE | TAnt | TPSO | TWPSO
    deriving(Show, Eq, Read)

-- | Información necesaria para llenar la base de datos.
type Statements = (Statement, Statement, Statement, [([SqlValue], [SqlValue])])

-- | Hace todas las inserciones necesarias en la base de datos.
insertParam :: IConnection conn
            => conn
            -> Statement        -- ^ Selección parámetros.
            -> Statement        -- ^ Inserción parámetros.
            -> Statement        -- ^ Inserción general.
            -> ([SqlValue], [SqlValue])
            -> IO ()
insertParam dbh s0 s1 s2 (l1, l2) = do
    n <- generalParam dbh s0 s1 l1
    case n of
        (Just n') -> do
            generalResult dbh s2 l2 n'
        Nothing   -> do
            putStrLn $ (show l1) ++ " | " ++ (show l2)

-- | Inserta los parámetros y devuelve el ID.
generalParam :: IConnection conn
             => conn               -- ^ Conexión.
             -> Statement          -- ^ Selección.
             -> Statement          -- ^ Inserción.
             -> [SqlValue]         -- ^ SqlValue.
             -> IO (Maybe Int)
generalParam dbh stmts stmti sv = do
    execute stmts sv
    mRes <- catchSql (fetchRow stmts) (\_ -> return Nothing)
    case mRes of
        Nothing    -> do
            execute stmti sv
            execute stmts sv
            mRes' <- catchSql (fetchRow stmts) (\_ -> return Nothing)
            case mRes' of
                Nothing      -> do
                    return Nothing
                (Just [x])   -> do
                    return $ Just (fromSql x)
        (Just [x]) -> do
            return $ Just (fromSql x)

-- | Introduce los resultados generales en la base de datos.
generalResult :: IConnection conn
              => conn                -- ^ Conexión.
              -> Statement           -- ^ Inserción.
              -> [SqlValue]          -- ^ Resultados generales.
              -> Int                 -- ^ Identificador.
              -> IO ()
generalResult dbh stmti sv id = do
    let sv' = sv ++ [toSql id]
    execute stmti sv'
    return ()
    `catchSql`
    (\_ -> putStrLn "Error")

-- "Parser"

-- | Parsea un documento.
parse :: IConnection conn
      => conn
      -> AlgType
      -> [String]
      -> IO (Statement, Statement, Statement, [([SqlValue], [SqlValue])])
parse dbh t str = do
    let l = map (parseLine t) str
    case t of
        TGA -> do
            stmt0 <- prepare dbh selectParamGA
            stmt1 <- prepare dbh insertParamGA
            stmt2 <- prepare dbh insertResultGA
            return (stmt0, stmt1, stmt2, l)
        TBee  -> do
            stmt0 <- prepare dbh selectParamBee
            stmt1 <- prepare dbh insertParamBee
            stmt2 <- prepare dbh insertResultBee
            return (stmt0, stmt1, stmt2, l)
        TDE   -> do
            stmt0 <- prepare dbh selectParamDE
            stmt1 <- prepare dbh insertParamDE
            stmt2 <- prepare dbh insertResultDE
            return (stmt0, stmt1, stmt2, l)
        TSDE  -> do
            stmt0 <- prepare dbh selectParamSDE
            stmt1 <- prepare dbh insertParamSDE
            stmt2 <- prepare dbh insertResultSDE
            return (stmt0, stmt1, stmt2, l)
        TAnt  -> do
            stmt0 <- prepare dbh selectParamAnt
            stmt1 <- prepare dbh insertParamAnt
            stmt2 <- prepare dbh insertResultAnt
            return (stmt0, stmt1, stmt2, l)
        TPSO  -> do
            stmt0 <- prepare dbh selectParamPSO
            stmt1 <- prepare dbh insertParamPSO
            stmt2 <- prepare dbh insertResultPSO
            return (stmt0, stmt1, stmt2, l)
        TWPSO -> do
            stmt0 <- prepare dbh selectParamWPSO
            stmt1 <- prepare dbh insertParamWPSO
            stmt2 <- prepare dbh insertResultWPSO
            return (stmt0, stmt1, stmt2, l)

-- | Parsea una línea.
parseLine :: AlgType -> String -> ([SqlValue], [SqlValue])
parseLine t l = (toDbType t f, toGeneral s)
    where (f, s) = toLists l

-- | Información general de la base de datos.
toGeneral :: [String] -> [SqlValue]
toGeneral (a:b:c:d:e:f:g:h:i:j:k:l:m:_) =
    [toSql (read a :: Int), toSql (read b :: Double), toSql (read c :: Double), toSql (read d :: Int), toSql (read e :: Int), toSql (read f :: Double), toSql (read g :: Double), toSql (read h :: Double), toSql (read i :: Int), toSql (read j :: Int), toSql (read k :: Double), toSql (read l :: Double), toSql (read m :: Double)]

-- | Tipo de la base de datos.
toDbType :: AlgType -> [String] -> [SqlValue]
toDbType TGA   (a:b:c:d:_)         = [toSql (read a :: Int), toSql (read b :: Int), toSql (read c :: Double), toSql (read d :: Double)]
toDbType TBee  (a:b:c:d:e:_)       = [toSql (read a :: Int), toSql (read b :: Int), toSql (read c :: Int), toSql (read d :: Int), toSql (read e :: Int)]
toDbType TDE   (a:b:c:d:_)         = [toSql (read a :: Int), toSql (read b :: Double), toSql (read c :: Double), toSql (read d :: Double)]
toDbType TSDE  (a:b:c:d:e:f:_)     = [toSql (read a :: Int), toSql (read b :: Double), toSql (read c :: Double), toSql (read d :: Double), toSql (read e :: Double), toSql (read f :: Double)]
toDbType TAnt  (a:_)               = [toSql (read a :: Int)]
toDbType TPSO  (a:b:c:d:e:_)       = [toSql (read a :: Int), toSql (read b :: Double), toSql (read c :: Double), toSql (read d :: Double), toSql (read e :: Double)]
toDbType TWPSO (a:b:c:d:e:f:g:h:_) = [toSql (read a :: Int), toSql (read b :: Double), toSql (read c :: Double), toSql (read d :: Double), toSql (read e :: Double), toSql (read f :: Double), toSql (read g :: Double), toSql (read h :: Double)]

-- | Separa la salida de los programas.
toLists :: String
        -> ([String], [String])
toLists l = (f', s')
    where (f, s) = span (/= '|') l
          f'     = filter ((flip (/=)) '-' .  head ) $ (read f :: [String])
          s'     = csvs $ tail $ tail s

-- | Separa los miembros de un CSV.
csvs :: String -> [String]
csvs s =  case dropWhile (==',') s of
            "" -> []
            s' -> w : csvs s''
                where (w, s'') = break (== ',') s'
