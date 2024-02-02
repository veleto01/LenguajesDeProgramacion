module EjerciciosClase
            (hayInterseccion,
            mayorDeTres,
            mayorDeTres',
            sumSubMul,
            concatMenorTres,
            getMayusculas,
            isLETres) where

import Data.Char
--Primera Clase
hayInterseccion :: (Int, Int) -> (Int, Int) -> Bool
hayInterseccion (x, y) (z,w) 
    | x < z && y <= z && z < w = False --(0,1) (1,2)
    | x <= z && y < z && z < w = False -- (1,0) (1,2)
    | x <= z && y < z && z > w = False --(1,0) (2,1)
    | x < z && y <= z && z > w = False --(0,1) (2,1)
    | otherwise = True

mayorDeTres :: Int -> Int -> Int -> Int
mayorDeTres x y z
    | x >= y && x >= z = x
    | y >= z = y
    | otherwise = z

--Segunda Clase
mayorDeTres' :: Int -> Int -> Int -> Int
mayorDeTres' x = max.max x

sumSubMul :: Int -> Int -> [Int]
sumSubMul x y = [x+y, x-y, x*y]

concatMenorTres :: String -> String -> String
concatMenorTres x y
    | length x <= 3 && length y <= 3 = x ++ y
    | otherwise = []

getMayusculas :: String -> String
getMayusculas xs = [x| x <- xs, isUpper x]

isLETres :: String -> Bool
isLETres (_:_:_:_:_) = False
isLETres _ = True