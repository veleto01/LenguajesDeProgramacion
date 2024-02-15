module Listado2Tema4 
            (sumaDobles,
            sumaDobles',
            sumaDoblesAux,
            sumaDobles'',
            sumaCuadradosParesFold,
            sumaCuadradosParesMapFilter,
            cuadrado,
            sumaCuadradosPares,
            eliminaValor,
            eliminaValor',
            eliminaDuplicados,
            divisores,
            isPrimo,
            listaPrimos,
            listaPrimos') where

sumaDobles :: [Int] -> Int
sumaDobles [] = 0
sumaDobles (x:xs) = 2*x + sumaDobles xs

sumaDobles' :: [Int] -> Int
sumaDobles' xs = sumaDoblesAux xs 0

sumaDoblesAux :: [Int] -> Int -> Int
sumaDoblesAux [] r = r
sumaDoblesAux (x:xs) r = sumaDoblesAux xs (2*x+r)

sumaDobles'' :: [Int] -> Int
sumaDobles'' = foldr (\x base -> base + 2*x) 0 

sumaCuadradosParesFold :: [Int] -> Int
sumaCuadradosParesFold = foldr (\x base -> if even x then x*x+base else base) 0

cuadrado :: Int -> Int
cuadrado x = x*x

sumaCuadradosParesMapFilter :: [Int] -> Int
sumaCuadradosParesMapFilter xs = sum(map cuadrado (filter even xs))

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = sum [x*x | x<-xs, even x]

eliminaValor :: Int -> [Int] -> [Int]
eliminaValor x = foldr (\n base -> if n == x then base else [n]++base) []

eliminaValor' :: Int -> [Int] -> [Int]
eliminaValor' x = foldl (\base n -> if n == x then base else base++[n]) []

eliminaDuplicados :: [Int] -> [Int]
eliminaDuplicados = foldl (\base n -> if elem n base then base else base++[n]) []

divisores :: Int -> [Int] 
divisores x = [y | y <- [1..x], x `mod` y == 0]

isPrimo :: Int -> Bool
isPrimo x = length (divisores x) == 2

listaPrimos :: [Int] -> [Int]
listaPrimos [] = []
listaPrimos (x:xs)
    | isPrimo x = x:listaPrimos xs
    | otherwise = listaPrimos xs

listaPrimos' :: [Int] -> [Int]
listaPrimos' xs = filter isPrimo xs