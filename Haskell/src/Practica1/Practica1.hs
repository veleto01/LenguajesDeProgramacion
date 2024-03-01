module Pratica1 
        (lecturaDatos,
        lineasFichero,
        partirLinea,
        partirLineaAux,
        parseContinente,
        parsePersonaje,
        personajesHistoricos,
        name',
        names,
        personajesProfesion,
        getCountry,
        personajesPais,
        getContinent,
        personajesContinenteProfesion,
        personajesHistoricosHombresYMujeres,
        personajesMayoresPais,
        getPaisesAux,
        getPaises,
        filtrarProfesionesAux,
        filtrarProfesiones,
        getProfesionesAux,
        getProfesiones
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

data Continente = Europe | Asia | Africa | NorthAmerica | Unknown | SouthAmerica deriving (Show, Eq)
type Ciudad = String
type Pais = String
data Lugar = L Ciudad Pais Continente deriving (Show)

data Personaje = P {name :: String, sex :: String, birth :: Int, place :: Lugar, occupation :: String} deriving Show


-- Recibe la linea y el delimitador
partirLinea :: String -> Char -> [String]
partirLinea linea d = partirLineaAux linea d [] []

partirLineaAux :: String -> Char -> String -> [String] -> [String]
partirLineaAux [] _ previous ac = ac ++ [previous]
partirLineaAux (x : xs) d palabra ac =
  if x == d
    then partirLineaAux xs d [] (ac ++ [palabra])
    else partirLineaAux xs d (palabra ++ [x]) ac

parseContinente :: String -> Continente
parseContinente c = case c of
  "Africa" -> Africa
  "Asia" -> Asia
  "North America" -> NorthAmerica
  "South America" -> SouthAmerica
  "Europe" -> Europe
  _ -> Unknown

parsePersonaje :: String -> Personaje
parsePersonaje line = P (datos !! 0) (datos !! 1) (read (datos !! 2)) (L (datos !! 3) (datos !! 4) (parseContinente (datos !! 5))) (datos !! 6)
  where
    datos = Prelude.take 7 (partirLinea line ';')

personajesHistoricos :: [Personaje]
personajesHistoricos = map parsePersonaje (tail lineasFichero)

name' :: Personaje -> String
name' (P n _ _ _ _) = n

names :: [Personaje] -> [String]
names lista = [name' p | p <- lista]

personajesProfesion :: [Personaje] -> String -> [Personaje]
personajesProfesion list occ = [p | p <- list, occupation p == occ]

getCountry :: Personaje -> String
getCountry (P _ _ _ (L _ p _) _) = p

personajesPais :: [Personaje] -> String -> [Personaje]
personajesPais list country = [p | p <- list, getCountry p == country]

getContinent :: Personaje -> Continente
getContinent (P _ _ _ (L _ _ c) _) = c

personajesContinenteProfesion :: [Personaje] -> String -> Continente -> [Personaje]
personajesContinenteProfesion list occ continent = [p | p <- list, occupation p == occ, getContinent p == continent]

personajesHistoricosHombresYMujeres :: [Personaje] -> String
personajesHistoricosHombresYMujeres list = "Hombres: " ++ show(length [p | p <- list, sex p == "Male"]) ++ " Mujeres: " ++ 
    show(length [p | p <- list, sex p == "Female"])

personajesMayoresPais :: [Personaje] -> Int -> String -> [Personaje]
personajesMayoresPais list birt country = [p | p <- list, getCountry p == country, birth p >= birt]

getPaisesAux :: [Personaje] -> [String] -> [String]
getPaisesAux [] r = r
getPaisesAux (x:xs) ac 
    | notElem (getCountry x) ac = getPaisesAux xs (ac ++ [getCountry x])
    | otherwise = getPaisesAux xs ac

getPaises :: [Personaje] -> [String]
getPaises list = getPaisesAux list []

filtrarProfesionesAux :: [Personaje] -> [String] -> [String]
filtrarProfesionesAux [] r = r
filtrarProfesionesAux (x:xs) ac 
    | notElem (occupation x) ac = filtrarProfesionesAux xs (ac ++ [occupation x])
    | otherwise = filtrarProfesionesAux xs ac

filtrarProfesiones :: [Personaje] -> [String]
filtrarProfesiones list = filtrarProfesionesAux list []

getProfesionesAux :: [Personaje] -> [String] -> [Int]
getProfesionesAux list prof = [length (personajesProfesion list p) | p <- prof]

getProfesiones :: [Personaje] -> [(String, Int)]
getProfesiones list = zip (profesiones) (getProfesionesAux list profesiones)
    where profesiones = filtrarProfesiones list