module Listado1Tema4   
            (cribar,
            cribar',
            cribar'',
            cribarAux,
            ceros,
            algunRepetido,
            quitarElemento,
            repeticiones,
            estaAlPrincipio,
            incluye,
            sumaCifrasString,
            sumaCifras,
            contieneCifraString,
            contieneCifra,
            invertirString,
            invertir,
            eliminarUltimos,
            eliminarUltimos',
            eliminarUltimosAux,
            eliminarUltimos'',
            listaOrdenada) where

cribar :: [Int] -> Int -> [Int]
cribar xs n = [x | x <- xs, not(x `mod` n == 0)]

cribar' :: [Int] -> Int -> [Int]
cribar' [] _ = []
cribar' (x:xs) n 
    | x `mod` n == 0 = cribar' xs n 
    | otherwise = x:cribar' xs n

cribar'' :: [Int] -> Int -> [Int]
cribar'' xs n = cribarAux xs n []

cribarAux :: [Int] -> Int -> [Int] -> [Int]
cribarAux [] _ r = reverse r
cribarAux (x:xs) n r
    | x `mod` n == 0 = cribarAux xs n r
    |otherwise = cribarAux xs n (x:r)

ceros :: [Int] -> Int
ceros [] = 0
ceros (x:xs)   
    | x == 0 && null xs = 1
    | x == 0 && not(head xs == 0) = 1 + ceros xs
    | otherwise = ceros xs

algunRepetido :: Int -> [Int] -> Bool
algunRepetido n xs = length [x | x <- xs, x == n] >= 1

quitarElemento :: [Int] -> Int -> [Int]
quitarElemento xs n = [x | x <- xs, not(x == n)]

repeticiones :: [Int] -> ([Int], [Int])
repeticiones [] = ([], [])
repeticiones (x:xs)
    | elem x xs = (unicos, x:repetidos)
    | otherwise = (x:unicos, repetidos)
    where (unicos, repetidos) = repeticiones (quitarElemento xs x)

estaAlPrincipio :: [Int] -> [Int] -> Bool
estaAlPrincipio [] _ = True
estaAlPrincipio _ [] = False
estaAlPrincipio (x:xs) (y:ys)
    | x == y = estaAlPrincipio xs ys
    | otherwise = False

incluye :: [Int] -> [Int] -> Bool
incluye [] _ = True
incluye _ [] = False
incluye xs ys@(_:zs)
    | estaAlPrincipio xs ys = True
    | otherwise = incluye xs zs

sumaCifrasString :: String -> Int
sumaCifrasString "" = 0
sumaCifrasString (c:cs) = (read [c] :: Int) + (sumaCifrasString cs)

sumaCifras :: Int -> Int
sumaCifras 0 = 0
sumaCifras x = sumaCifrasString (show x)

contieneCifraString :: String -> String -> Bool
contieneCifraString _ "" = False
contieneCifraString c (c1:cs)
    | c == [c1] = True
    | otherwise = contieneCifraString c cs

contieneCifra :: Int -> Int -> Bool
contieneCifra x y= contieneCifraString (show x) (show y)

invertirString :: String -> Int
invertirString "" = 0
invertirString (c:cs) = read (show (invertirString cs) ++ [c])

invertir :: Int -> Int
invertir x = invertirString (show x)

eliminarUltimos :: Int -> [Int] -> [Int]
eliminarUltimos n xs = take (length xs - n) xs

eliminarUltimos' :: Int -> [Int] -> [Int]
eliminarUltimos' _ [] = []
eliminarUltimos' n (x:xs)
    | null xs = []
    | length xs >= n = x:eliminarUltimos' n xs
    | otherwise = []

eliminarUltimosAux :: Int -> [Int] -> [Int] -> [Int]
eliminarUltimosAux _ [] r = r
eliminarUltimosAux n (x:xs) r 
    | null xs = r
    | length xs >= n = eliminarUltimosAux n xs (r++[x])
    | otherwise = r

eliminarUltimos'' :: Int -> [Int] -> [Int]
eliminarUltimos'' n xs = eliminarUltimosAux n xs []

listaOrdenada :: [Int] -> Bool
listaOrdenada [] = True
listaOrdenada [_] = True
listaOrdenada (x:y:xs)
    | x <= y = True && listaOrdenada (y:xs)
    | otherwise = False

