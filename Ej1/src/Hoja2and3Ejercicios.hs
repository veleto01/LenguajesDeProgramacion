module Hoja2Ejercicios 
            (cocienteResto,
            maximoTupla,
            componer,
            impar, 
            signum',
            signum'',
            bisiesto,
            sumaCuatupla,
            ordenadosMenor,
            ordenarTupla,
            getDecimal, 
            descomponerReal,
            divisores,
            esDigito,
            esDigito',
            esPrimo,
            listaPrimosImpares,
            primosMenorIgual,
            isVocal,
            codificacionTuplas,
            isMayor,
            filtrarTuplas,
            isPitagorica,
            cuantasPitagoricas,
            esMayuscula,
            mayusculasMinusculas,
            listaASCII,
            mensajeLista,
            contarMayusculas) where

import Data.Char

cocienteResto :: Int -> Int -> (Int, Int)
cocienteResto x y = (x `div` y, x `mod` y)

maximoTupla :: (Int,Int) -> Int
maximoTupla (x,y) = max x y 

componer :: Int -> Int -> Int
componer x = maximoTupla.cocienteResto x

--componer x y = max (x `div` y) (x `mod` y) Esta bien

--componer x = max (x `div` ) . mod x No se puede hacer por composicion de funciones ya que necesito ambos valores en dos funciones diferentes que se utilizan para el max por lo que si pongo x `div` y tengo que poner obligatoriametne
-- x `mod` y lo que dejaria de ser una funcion y pasaria a ser un resultado impidiendo composicion de funciones

--componer x = max 4 . mod x 
--Aplica y a mod x mod x y , aplica resultado a max 4 // funcion equivalente max 4 (mod x y)

impar :: Int -> Bool
impar = not.even

signum' :: Int -> Int
signum' x 
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

signum'' :: Int -> Int
signum'' x = if x > 0 then 1 else if x < 0 then -1 else 0

bisiesto :: Int -> Bool
bisiesto x = x `mod` 4 == 0

sumaCuatupla :: (Int, Int, Int, Int) -> Int
sumaCuatupla (x, y, z, w) = x + y + z + w

ordenadosMenor :: Int -> Int -> Int -> Bool
ordenadosMenor x y z = x<=y && y <= z

ordenarTupla :: (Int, Int, Int) -> (Int, Int, Int)
ordenarTupla (x, y, z) 
    | x <= y && y <= z = (x, y, z)
    | x <= z && z <= y = (x, z, y)
    | y <= x && x <= z = (y, x, z)
    | y <= z && z <= x = (y, z, x)
    | z <= x && x <= y = (z, x, y)
    | otherwise = (z, y, x)

getDecimal :: Float -> Int
getDecimal x = mod (round (x * 100)) 100

descomponerReal :: Float -> (Int, Int)
descomponerReal x = (truncate x, getDecimal x)

divisores :: Int -> [Int] 
divisores x = [y | y <- [1..x], x `mod` y == 0]

esDigito :: Char -> Bool
esDigito x 
    | isDigit x = True
    | otherwise = False

esDigito' :: Char -> Bool
esDigito' '1' = True
esDigito' '2' = True
esDigito' '3' = True
esDigito' '4' = True
esDigito' '5' = True
esDigito' '6' = True
esDigito' '7' = True
esDigito' '8' = True
esDigito' '9' = True
esDigito' _ = False

esPrimo :: Int -> Bool
esPrimo x = length (divisores x) == 2

listaPrimosImpares :: [Int] -> [Int]
listaPrimosImpares [] = []
listaPrimosImpares x = [y | y <- x, esPrimo y && odd y]

primosMenorIgual :: Int -> [Int]
primosMenorIgual x = [y | y <- [1..x], esPrimo y]

isVocal :: Char -> Bool
isVocal 'a' = True
isVocal 'e' = True
isVocal 'i' = True
isVocal 'o' = True
isVocal 'u' = True
isVocal _ = False

codificacionTuplas :: [(Char, Char)] -> String
codificacionTuplas [] = ""
codificacionTuplas cs = [fst s | s <- cs, isVocal(snd s)]

isMayor :: Int -> Int -> Bool
isMayor x n 
    | x >= n = True
    | otherwise = False

filtrarTuplas :: [(Int, Int)] -> Int -> [(Int, Int)]
filtrarTuplas [] _ = []
filtrarTuplas xs n = [y | y <- xs, isMayor (fst y) n, odd (fst y)]

isPitagorica :: (Int, Int, Int) -> Bool
isPitagorica (x, y, z) = (x*x + y*y) == z*z

cuantasPitagoricas :: [(Int, Int, Int)] -> Int
cuantasPitagoricas [] = 0
cuantasPitagoricas xs = length[y | y <- xs, isPitagorica y]

esMayuscula :: Char -> Bool
esMayuscula c = elem c ['A'..'Z']

mayusculasMinusculas :: String -> String
mayusculasMinusculas cs = [if esMayuscula c then toLower c else toUpper c | c <- cs]

listaASCII :: String -> [Int]
listaASCII cs = [ord x | x <- cs]

mensajeLista :: [Int] -> String
mensajeLista [] = "Lista vacia"
mensajeLista (x:xs) = "Primer elemento: " ++ show x ++ ", longitud: " ++ show (length xs + 1)

contarMayusculas :: String -> Int
contarMayusculas [] = 0
contarMayusculas cs = length[s | s <- cs, esMayuscula s]