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
    s <- readFile fp
    let a = parse t $ lines s
    mapM_ (insertParam dbh) a
    commit dbh

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

-- | Resultados Generales.
data General = Ge { ci       :: Int
                  , alg_time :: Double
                  , hib_time :: Double
                  , cf       :: Int
                  , alg_eval :: Int
                  , alg_fo   :: Double
                  , alg_db   :: Double
                  , alg_je   :: Double
                  , km_eval  :: Int
                  , hib_eval :: Int
                  , hib_fo   :: Double
                  , hib_db   :: Double
                  , hib_je   :: Double
                  }
    deriving(Show, Eq)

-- | Parámetros.
data DbType = GA   Int Int Double Double
            | Bee  Int Int Int Int Int
            | DE   Int Double Double Double
            | SDE  Int Double Double Double Double Double
            | Ant  Int
            | PSO  Int Double Double Double Double
            | WPSO Int Double Double Double Double Double Double Double
    deriving(Show, Eq)

-- | Algoritmo.
type Alg = (DbType, General)

-- | Tipos de algoritmos.
data AlgType = TGA | TBee | TDE | TSDE | TAnt | TPSO | TWPSO
    deriving(Show, Eq, Read)

-- | Inserta los parámetros y devuelve el ID. Depende del tipo de algoritmo.
insertParam :: IConnection conn
             => conn
             -> Alg
             -> IO ()
insertParam dbh ((GA i tt pc pm), g) = do
    n <- generalParam dbh selectParamGA insertParamGA [toSql i, toSql tt, toSql pc, toSql pm]
    case n of
        (Just n') -> do
            generalResult dbh insertResultGA g n'
        Nothing   -> do
            putStrLn $ (show (GA i tt pc pm) ) ++ " | " ++ (show g)
insertParam dbh ((Bee i m e eb ob), g) = do
    n <- generalParam dbh selectParamBee insertParamBee [toSql i, toSql m, toSql e, toSql eb, toSql ob]
    case n of
        (Just n') -> do
            generalResult dbh insertResultBee g n'
        Nothing   -> do
            putStrLn $ (show (Bee i m e eb ob) ) ++ " | " ++ (show g)
insertParam dbh ((DE i w1 w2 w3), g) = do
    n <- generalParam dbh selectParamDE insertParamDE [toSql i, toSql w1, toSql w2, toSql w3]
    case n of
        (Just n') -> do
            generalResult dbh insertResultDE g n'
        Nothing   -> do
            putStrLn $ (show (DE i w1 w2 w3) ) ++ " | " ++ (show g)
insertParam dbh ((SDE i w1 w2 w3 f pc), g) = do
    n <- generalParam dbh selectParamSDE insertParamSDE [toSql i, toSql w1, toSql w2, toSql w3, toSql f, toSql pc]
    case n of
        (Just n') -> do
            generalResult dbh insertResultSDE g n'
        Nothing   -> do
            putStrLn $ (show (SDE i w1 w2 w3 f pc) ) ++ " | " ++ (show g)
insertParam dbh ((Ant i), g) = do
    n <- generalParam dbh selectParamAnt insertParamAnt [toSql i]
    case n of
        (Just n') -> do
            generalResult dbh insertResultAnt g n'
        Nothing   -> do
            putStrLn $ (show (Ant i) ) ++ " | " ++ (show g)
insertParam dbh ((PSO i c1 c2 w vmx), g) = do
    n <- generalParam dbh selectParamPSO insertParamPSO [toSql i, toSql c1, toSql c2, toSql w, toSql vmx]
    case n of
        (Just n') -> do
            generalResult dbh insertResultPSO g n'
        Nothing   -> do
            putStrLn $ (show (PSO i c1 c2 w vmx) ) ++ " | " ++ (show g)
insertParam dbh ((WPSO i c1 c2 w vmx w1 w2 w3), g) = do
    n <- generalParam dbh selectParamWPSO insertParamWPSO [toSql i, toSql c1, toSql c2, toSql w, toSql vmx, toSql w1, toSql w2, toSql w3]
    case n of
        (Just n') -> do
            generalResult dbh insertResultWPSO g n'
        Nothing   -> do
            putStrLn $ (show (WPSO i c1 c2 w vmx w1 w2 w3) ) ++ " | " ++ (show g)
    

-- | Inserta los parámetros y devuelve el ID.
generalParam :: IConnection conn
             => conn               -- ^ Conexión.
             -> String             -- ^ Selección.
             -> String             -- ^ Inserción.
             -> [SqlValue]         -- ^ SqlValue.
             -> IO (Maybe Int)
generalParam dbh s i sv = do
    res <- catchSql (quickQuery dbh s sv)
                    (\_ -> return [])
    case res of
        []      -> do
            catchSql (do {run dbh i sv; return ()}) (\e -> return ())
            res' <- catchSql (quickQuery dbh s sv)
                             (\_ -> return [])
            case res' of
                []      -> do
                    return Nothing
                ([x]:_) -> do
                    return $ Just (fromSql x)
        ([x]:_) -> do
            return $ Just (fromSql x)

-- | Introduce los resultados generales en la base de datos.
generalResult :: IConnection conn
              => conn                -- ^ Conexión.
              -> String              -- ^ Inserción.
              -> General             -- ^ Resultados generales.
              -> Int                 -- ^ Identificador.
              -> IO ()
generalResult dbh ins (Ge a b c d e f g h i j k l m) id = do
    run dbh ins [toSql a, toSql b, toSql c, toSql d, toSql e, toSql f, toSql g, toSql h, toSql i, toSql j, toSql k, toSql l, toSql m, toSql id]
    return ()
    `catchSql`
    (\_ -> putStrLn "Error")

-- "Parser"

-- | Parsea un documento.
parse :: AlgType -> [String] -> [Alg]
parse t = map (parseLine t)

-- | Parsea una línea.
parseLine :: AlgType -> String -> Alg
parseLine t l = (toDbType t f, toGeneral s)
    where (f, s) = toLists l

-- | Información general de la base de datos.
toGeneral :: [String] -> General
toGeneral (a:b:c:d:e:f:g:h:i:j:k:l:m:_) =
    (Ge (read a :: Int) (read b :: Double) (read c :: Double) (read d :: Int) (read e :: Int) (read f :: Double) (read g :: Double) (read h :: Double)  (read i :: Int) (read j :: Int) (read k :: Double) (read l :: Double) (read m :: Double))

-- | Tipo de la base de datos.
toDbType :: AlgType -> [String] -> DbType
toDbType TGA   (a:b:c:d:_)         = GA (read a :: Int) (read b :: Int) (read c :: Double) (read d :: Double)
toDbType TBee  (a:b:c:d:e:_)       = Bee (read a :: Int) (read b :: Int) (read c :: Int) (read d :: Int) (read e :: Int)
toDbType TDE   (a:b:c:d:_)         = DE (read a :: Int) (read b :: Double) (read c :: Double) (read d :: Double)
toDbType TSDE  (a:b:c:d:e:f:_)     = SDE (read a :: Int) (read b :: Double) (read c :: Double) (read d :: Double) (read e :: Double) (read f :: Double)
toDbType TAnt  (a:_)               = Ant (read a :: Int)
toDbType TPSO  (a:b:c:d:e:_)       = PSO (read a :: Int) (read b :: Double) (read c :: Double) (read d :: Double) (read e :: Double)
toDbType TWPSO (a:b:c:d:e:f:g:h:_) = WPSO (read a :: Int) (read b :: Double) (read c :: Double) (read d :: Double) (read e :: Double) (read f :: Double) (read g :: Double) (read h :: Double)

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
