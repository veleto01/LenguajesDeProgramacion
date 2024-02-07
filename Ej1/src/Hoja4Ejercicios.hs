module Hoja4Ejercicios 
            (isSameChar,
            contarApariciones,
            manipula3Tuplas,
            sumaMenor10,
            puntoCardinal,
            todosIguales,
            mensajeFrase,
            clasificarValorEntrada,
            divisores,        
            amigos,
            isVocal,
            isConsonante,
            contarConsonantes,
            esPrimo,
            isMersenne,
            listaMersenne,
            listaATrue,
            listasIguales,
            head',
            tail',
            mayorDivision,
            sumaTipos,
            cuadruple,
            calificacion,
            cuadrado,
            posicionEnLista,
            long,
            contiene,
            primeros,
            partir,
            insertar,
            segundos,
            codifica,
            unionLista,
            listaPotencias,
            isPerfecto,
            listaPerfectos,
            isFermat,
            listaFermat) where

import Data.Char

isSameChar :: Char -> Char -> Bool
isSameChar c1 c2 = c1 == c2

contarApariciones :: String -> Char -> Int
contarApariciones "" _ = 0
contarApariciones (c1:cs) c
    | isSameChar c1 c = length [x | x <- cs, isSameChar x c] + 1
    | otherwise = length [x | x <- cs, isSameChar x c]

manipula3Tuplas :: ((String, Int), (String, Int), (String, Int)) -> (String, String, String)
manipula3Tuplas ((s1, _), (s2, _), (s3, _)) = (s1, s2, s3)

--Tomando como precondicion que el length sea > 4
sumaMenor10 :: [Int] -> Bool
sumaMenor10 (x:y:z:w:_) = x+y+z+w < 10 
sumaMenor10 _ = False 

puntoCardinal :: Char -> String
puntoCardinal 'N' = "Norte"
puntoCardinal 'S' = "Sur"
puntoCardinal 'E' = "Este"
puntoCardinal 'O' = "Oeste"
puntoCardinal _ = "El caracter introducido no pertenece a un punto cardinal"

todosIguales :: Int -> [Int] -> Bool
todosIguales y xs = length [x | x <- xs, x == y] == length xs

mensajeFrase :: String -> String
mensajeFrase s = "La primera letra de la frase " ++ s ++ " es " ++ show(head s) ++ 
    " y la ultima letra es " ++ show(last s)

clasificarValorEntrada :: Int -> String
clasificarValorEntrada x 
    | x < v1 = "El valor de entrada es menor o igual a 10"
    | v1 <= x && x <= v2 = "El valor de entrada es mayor o igual a 10 y menor o igual a 20"
    | otherwise = "El valor de entrada es mayor a 20"
        where
            v1 = 10
            v2 = 20  

divisores :: Int -> [Int] 
divisores x = [y | y <- [1..x], x `mod` y == 0]

amigos :: (Int, Int) -> Bool
amigos (n1,n2) = sum(divisores n1) == sum(divisores n2)

isVocal :: Char -> Bool
isVocal 'a' = True
isVocal 'e' = True
isVocal 'i' = True
isVocal 'o' = True
isVocal 'u' = True
isVocal 'A' = True
isVocal 'E' = True
isVocal 'I' = True
isVocal 'O' = True
isVocal 'U' = True
isVocal _ = False

isConsonante :: Char -> Bool
isConsonante c = isLetter c && not(isVocal c)

contarConsonantes :: String -> Int
contarConsonantes s = length[c | c <- s, isConsonante c]

esPrimo :: Int -> Bool
esPrimo x = length (divisores x) == 2

isMersenne :: Int -> Bool
isMersenne x = if x `elem` [2^n - 1 | n <- [1..100]] then esPrimo x else False

listaMersenne :: Int -> [Int]
listaMersenne n = take n [x | x <- [1..], isMersenne x]

listaATrue :: [Bool] -> Bool
listaATrue xs = length [x | x <- xs, x == True] == length xs

--Asumimos que tienen la misma longitud
listasIguales :: [Int] -> [Int] -> Bool
listasIguales xs ys = listaATrue [fst x == snd x | x <- zip xs ys]

head' :: [Int] -> Int
head' [] = 0
head' (x:_) = x

tail' :: [Int] -> [Int]
tail' [] = []
tail' (_:xs) = xs

mayorDivision :: Int -> Int -> Int
mayorDivision x y = max (x `div` y) (x `mod` y)

sumaTipos :: Integer -> Float -> Float
sumaTipos x y = fromInteger x + y

cuadruple :: Int -> Int
cuadruple x = 4*x

calificacion :: Float -> String
calificacion x 
    | x>= v0 && x < v1 = "Suspenso"
    | v1 <= x && x < v2 = "Aprobado"
    | v2 <= x && x < v3 = "Notable"
    | v3 <= x && x < v4 = "Sobresaliente"
    | x == v4 = "Matricula Honor"
    | otherwise = "Nota no valida"
        where
            v0 = 0
            v1 = 5
            v2 = 7
            v3 = 9
            v4 = 10

cuadrado :: [Int] -> [Int]
cuadrado xs = [x*x | x <- xs, even x]

posicionEnLista :: [Int] -> [(Int, Int)]
posicionEnLista xs = zip xs [x | x <- [0..length xs-1]]

long :: [Int] -> Int
long xs = sum [1 | _ <- xs] 

contiene :: Int -> [Int] -> Bool
contiene x xs = length [n | n <- xs, n == x] == 1

primeros :: [(Char, Int)] -> String
--primeros cs = [fst c | c <- cs] Opcion completamente valida
primeros = fst.unzip

partir :: Int -> [Int] -> ([Int], [Int])
partir n xs = (take n xs, drop n xs)

insertar :: [Int] -> Int -> Int -> [Int]
insertar xs n p = take p xs ++ n:[] ++ drop p xs

segundos :: [(Int, Int)] -> [Int]
segundos = snd.unzip

codifica :: [Int] -> String
codifica xs = [if c `mod` 2 == 0 then 'p' else 'i' | c <- segundos(posicionEnLista xs)]

unionLista :: [Int] -> Int -> [(Int, Int)]
unionLista xs x = zip xs (reverse[0..x-1])

listaPotencias :: [Int] -> [Int]
listaPotencias xs = [x^y | (x,y) <- unionLista xs (length xs)]

isPerfecto :: Int -> Bool
isPerfecto n = sum(init (divisores n)) == n

listaPerfectos :: Int -> [Int]
listaPerfectos x = take x [n | n <- [1..], isPerfecto n]

isFermat :: Int -> Bool
isFermat x = if x `elem` [2^n + 1 | n <- [1..100]] then esPrimo x else False

listaFermat :: Int -> [Int]
listaFermat x = take x [n | n <- [1..], isFermat n]