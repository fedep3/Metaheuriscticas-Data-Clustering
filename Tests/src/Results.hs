module Results (
    parseResult
)where

import Text.ParserCombinators.Parsec
import Data.List

-- | Obtiene los resultados.
getResult = do
    ls <- many1 line
    return $ intercalate " " $ filter (/= "") ls

-- | Tipos de línea.
line = try (result)
     <|> (estorbo)
     <?> "line"

-- | Líneas que no sirven para el resultado.
estorbo = do
    many1 $ noneOf "\n"
    newline
    return ""

-- | Header de resultado.
header = do
    many1 $ noneOf ":"
    char ':'
    spaces

-- | Una línea de resultado.
result = do
    header
    r <- number
    many $ noneOf "\n"
    newline
    return r

-- | Tipos de números.
number = try (flotante)
         <|> (entero)
         <?> "number"

-- | Número punto flotante.
flotante = do
    s <- sign
    l <- many digit
    p <- char '.'
    r <- many1 digit
    c <- scientific <|> (return "") <?> "scientific notation"
    return $ s ++ l ++ [p] ++ r ++ c
    
-- | Notación científica
scientific = do
    e <- char 'e'
    s <- sign
    d <- many1 digit
    return $ e : (s ++ d)

-- Número entero.
entero = do
    s <- sign
    d <- many1 digit
    return $ s ++ d

-- | Signo del número.
sign = generalSign '-'
       <|> generalSign '+'
       <|> return ""
       <?> "sign"

-- | Signo del número.
generalSign s = do
    s' <- char s
    return [s']

-- | Parser de los resultados.
parseResult = parse getResult "(stdin)"
