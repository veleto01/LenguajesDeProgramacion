module Pratica1 
        (lecturaDatos,
        lineasFichero,
        mapFicheroToPersonajeFinal,
        mapFicheroToPersonaje,
        split
        ) where
        
import System.IO()
-- Necesario para hacer el casting de IO [a] -> [a]
import System.IO.Unsafe

lecturaDatos :: IO [String]
lecturaDatos = do
    contents <- readFile "personajes.csv"
    return (lines contents)


-- Recogida de datos desde IO
lineasFichero :: [String]
lineasFichero = unsafePerformIO (lecturaDatos)

data Continente = Europe | Asia | Africa | NorthAmerica | Unknown | SouthAmerica deriving (Show)
type Ciudad = String
type Pais = String
data Lugar = L Ciudad Pais Continente deriving (Show)

data Personaje = Personaje {name :: String, sex :: String, birth :: Int, place :: Lugar, occupation :: String} deriving Show

split :: String -> Char -> [String]
split s d = splitAux s d [] [] 

splitAux :: String -> Char -> String -> [String] -> [String]
splitAux [] _ prev r = r++prev
splitAux (c:cs) delim palab r 
    | c == delim = splitAux cs delim [] r++[palab]
    | otherwise = splitAux cs delim (c:palab) r


mapFicheroToPersonajeFinal :: [String] -> [Personaje] -> [Personaje]
mapFicheroToPersonajeFinal [] r = r
mapFicheroToPersonajeFinal (x:xs) r = mapFicheroToPersonajeFinal xs (Personaje):r

mapFicheroToPersonaje :: [String] -> [Personaje]
mapFicheroToPersonaje s = mapFicheroToPersonajeFinal s

