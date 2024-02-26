import System.IO

-- Necesario para hacer el casting de IO [a] -> [a]
import System.IO.Unsafe

data ContinenteString = C String deriving Show
data Continente = Europe | Asia | Africa | NorthAmerica | Unknown | SouthAmerica deriving (Show, Eq)
type Ciudad = String
type Pais = String
data Lugar = L Ciudad Pais Continente deriving (Show)

lecturaDatos :: IO [String]
lecturaDatos = do
    contents <- readFile "personajes.csv"
    return (lines contents)

-- Recogida de datos desde IO
lineasFichero :: [String]
lineasFichero = unsafePerformIO (lecturaDatos)

-- 0. Funciones auxiliares

type Nombre = String
type Sexo = String
type AñoNacimiento = Int
type Ocupacion = String
data Personaje = P Nombre Sexo AñoNacimiento Lugar Ocupacion deriving (Show)

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

name :: Personaje -> String
name (P n _ _ _ _) = n

names :: [Personaje] -> [String]
names lista = [name p | p <- lista]

--Dada una profesión, mostrar todos los personajes que sean de esa profesión.
profesion:: String -> [Personaje]-> [Personaje]
profesion _ [] = []
profesion  profesionDada (p@(P a v b c comprobarProfesion):xs) = if comprobarProfesion == profesionDada then [p]++profesion profesionDada xs else profesion profesionDada xs

--Dado un país, mostrar todos los personajes de ese país.
pais:: String -> [Personaje]-> [Personaje]
pais _ [] = []
pais paisDada (persona@(P nombre sexo añoNacimiento paisSeleccionado@(L _ p _ ) ocupacion ):xs) = if p == paisDada then [persona]++pais paisDada xs else pais paisDada xs

--Dado un continente y una profesión, mostrar todos los personajes de esa profesión y del mismo continente.

ejercicio3 :: Continente -> String -> [Personaje] -> [Personaje]
ejercicio3 _ _ [] = []
ejercicio3 continente profesion (persona@(P nombre sexo añoNacimiento continentePersona@(L _ _ continentesString) ocupacion ):xs) = if (profesion == ocupacion && continente == continentesString) then [persona]++ejercicio3 continente profesion xs else ejercicio3 continente profesion xs


--Devolver el número de personajes históricos hombres y mujeres, es decir, sabercuántos hay de cada tipo. 

hombres :: [Personaje] -> Int
hombres [] = 0
hombres (persona@(P nombre sexo añoNacimiento continente ocupacion ):xs) = if sexo == "Male" then  1+hombres xs else hombres xs

mujeres :: [Personaje] -> Int
mujeres [] = 0
mujeres (persona@(P nombre sexo añoNacimiento continente ocupacion ):xs) = if sexo == "Female" then  1+mujeres xs else mujeres xs

hombresMujeres :: [Personaje] -> (Int, Int)
hombresMujeres lista =   (hombres lista,mujeres lista)