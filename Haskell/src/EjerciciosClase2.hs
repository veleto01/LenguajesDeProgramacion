module EjerciciosClase2
            (comienzaMayus,
            comienzaPorA,
            ordenarCadenas,
            charDeComienzo,
            longitud,
            longitud',
            longitud'',
            contarCaracteres,
            contarCaracteres',
            contarCaracteres2',
            sum',
            sum'',
            sumaListaFinal,
            length',
            invertirLista,
            mapVariasFunciones,
            mapVariasFunciones',
            maxValueList,
            dosVeces,
            mezclar,
            init',
            last',
            zipWith') where

import Data.Char

comienzaMayus :: String -> Bool
comienzaMayus (c:_) = c `elem` ['A'..'Z'] 
comienzaMayus _ = False

comienzaPorA :: String -> Bool
comienzaPorA (c:_) = c == 'a' || c == 'A'
comienzaPorA _ = False

charDeComienzo :: String -> Char
charDeComienzo (c:_) = c
charDeComienzo _ = '\t'

ordenarCadenas :: String -> String -> (String, String)
ordenarCadenas "" s = ("", s);
ordenarCadenas s "" = ("", s);
ordenarCadenas s1 s2
    | toLower(charDeComienzo s1) < toLower(charDeComienzo s2) = (s1, s2)
    | otherwise = (s2, s1)

--Recursividad

longitud :: [Int] -> Int
longitud xs = if null xs then 0 else 1+longitud(tail xs)

longitud' :: [Int] -> Int
longitud' [] = 0
longitud' (_:xs) = 1 + longitud' xs

longitud'' :: [Int] -> Int
longitud'' xs = case null xs of
    True -> 0
    _ -> 1 + longitud'' (tail xs)

contarCaracteres :: String -> Char -> Int
contarCaracteres [] _ = 0
contarCaracteres (c1:cs) c 
    | c1 == c = 1 + contarCaracteres cs c
    | otherwise = contarCaracteres cs c

contarCaracteres' :: String -> Char -> Int
contarCaracteres' "" _ = 0
contarCaracteres' (c1:cs) c 
    | c1 == c = contarCaracteres2' cs c 1
    | otherwise = contarCaracteres2' cs c 0

contarCaracteres2' :: String -> Char -> Int -> Int
contarCaracteres2' "" _ r = r
contarCaracteres2' (c:cs) ch r = if c == ch then contarCaracteres2' cs ch (r+1) else contarCaracteres2' cs ch r

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' :: [Int] -> Int
sum'' xs = sumaListaFinal xs 0

sumaListaFinal :: [Int] -> Int -> Int
sumaListaFinal [] r = r
sumaListaFinal (x:xs) r = sumaListaFinal xs (r+x)

length' :: [Int] -> Int
length' = foldr (\_ y -> y+1) 0

invertirLista :: [Int] -> [Int]
invertirLista = foldl (\lista x -> [x] ++ lista) []

mapVariasFunciones :: [Int -> Int] -> Int -> [Int]
mapVariasFunciones [] _ = []
mapVariasFunciones (f:fs) n = [f n] ++ (mapVariasFunciones fs n)

mapVariasFunciones' :: [(Int -> Int)] -> Int -> [Int]
mapVariasFunciones' fs n = foldr (\f lista -> (f n):lista) [] fs

maxValueList :: [Int] -> Int
maxValueList xs = foldl (\base n -> max base n) (head xs) (tail xs)  

dosVeces :: (a -> a) -> a -> a
dosVeces f x = f (f x)

mezclar :: [(a,b)] -> [(c,d)] -> [((a,c),(b,d))]
mezclar [] _  = []
mezclar _ [] = []
mezclar ts t2s = zip (zip (fst(unzip ts)) (fst(unzip t2s))) (zip (snd(unzip ts)) (snd(unzip t2s)))

init' :: [a] -> [a]
init' [] = error "empty list"
init' [_] = []
init' (x:xs) = x:init' xs

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y):zipWith' f xs ys