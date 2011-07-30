{-# LANGUAGE BangPatterns #-}
module Files (
    FileHandles(..),
    runTests
) where

import Pruebas
import Results
import System.IO
import Data.Array
import System.Posix.Signals
import qualified Data.List as DL
import qualified System.Process as SP
import qualified System.Directory as SD
import qualified Data.ByteString.Char8 as BS

-- | Descriptores de los archivos.
data FileHandles = FH { hlog    :: Handle
                      , hinfo   :: Handle
                      , hresult :: Handle
                      }

-- | Si la corrida ha sido terminada o no.
data RunState = FINISHED | UNFINISHED
    deriving(Eq, Show, Read)

-- | Linea de la corrida.
type Log = (RunState, String, Int)

-- | Estado actual de las pruebas.
type CurrentState = (FileHandles, Int)

-- | Signal Handler del cierre de archivos.
endTester :: Handle -- * Handle del log.
          -> Handle -- * Handle del .info.
          -> Handle -- * Handle del .result.
          -> IO ()
endTester lh li lr = do
    putStrLn "Finalizando..."
    hClose lh
    hClose li
    hClose lr

-- | Inicializa el programa de pruebas.
getState :: FilePath        -- * Archivo de log. 
         -> String          -- * Nombre de la prueba.
         -> FilePath        -- * Opciones de corrida.
         -> IO CurrentState -- * Estado actual del programa.
getState l n fsource = do
    let finfo   = (n ++ ".info")
    let fresult = (n ++ ".result")
    bh          <- SD.doesFileExist l
    case bh of
        True -> do
            putStrLn "Leyendo Log..."
            logs      <- BS.readFile l
            let logs'  = reverse $ map (\x -> read x :: Log) $ lines $ BS.unpack logs
            let last   = head $ filter (\(u, s, _) -> finfo == s) logs'
            case last of
                (FINISHED, _, _)   -> do
                    putStrLn $ "No hay corridas pendientes con el nombre: " ++ n
                    putStrLn $ "Generando nueva corrida..."
                    -- Generar un nuevo log, archivo .info y .result y ejecutar.
                    os <- BS.readFile fsource
                    let os' = (read (BS.unpack os) :: [Options])
                    let cmds = BS.pack $ unlines $ concat $ map (genPrograms) os'
                    BS.writeFile finfo cmds
                    lh <- openFile l AppendMode
                    li <- openFile finfo ReadMode
                    lr <- openFile fresult WriteMode
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
                    lr <- openFile fresult AppendMode
                    installHandler sigINT (Catch $ endTester lh li lr) Nothing
                    return $ ((FH lh li lr), i)
        False -> do
            putStrLn $ "Generando log y nueva corrida..."
            -- Generar un nuevo log, archivo .info y .result y ejecutar.
            os <- BS.readFile fsource
            let os' = (read (BS.unpack os) :: [Options])
            let cmds = BS.pack $ unlines $ concat $ map (genPrograms) os'
            BS.writeFile finfo cmds
            lh  <- openFile l AppendMode
            li  <- openFile finfo ReadWriteMode
            lr  <- openFile fresult WriteMode
            installHandler sigINT (Catch $ endTester lh li lr) Nothing
            hPutStrLn lh $ show $ (UNFINISHED, finfo, 0)
            return $ ((FH lh li lr), 0)

-- | Genera y corre las pruebas.
runTests l n fsource = do
    (hs, i)   <- getState l n fsource
    !cmds      <- BS.hGetContents $ hinfo hs
    let cmds'  = lines $ BS.unpack cmds
    let cmds'' = listArray (0, (length cmds' - 1)) cmds'
    let top    = 1 + (snd $ bounds cmds'')
    run l n hs cmds'' (i , top)
    putStrLn "Liberando recursos..."
    hClose $ hlog hs
    hClose $ hinfo hs
    hClose $ hresult hs
    putStrLn "Finalizado =D"

-- | Ejecuta los programas.
run :: FilePath
    -> String
    -> FileHandles
    -> Array Int String
    -> (Int, Int)
    -> IO ()
run l n hs cmds (i, f) = do
    case (i == f) of
        True  -> do
            hPutStrLn (hlog hs) $ show $ (FINISHED, (n ++ ".info"), i)
            return ()
        False -> do
            let (e:o) = words $ cmds ! i
            s <- SP.readProcess e o ""
            let s' = case parseResult s of
                        Left c  -> "Hubo un error al recolectar los datos."
                        Right r -> r
            hPutStr (hresult hs) $ (show $ snd $ DL.break (== "--i") $ words (cmds ! i) ) ++ " | "
            hPutStrLn (hresult hs) s'
            hPutStrLn (hlog hs) $ show $ (UNFINISHED, (n ++ ".info"), i + 1)
            run l n hs cmds (i + 1, f)
