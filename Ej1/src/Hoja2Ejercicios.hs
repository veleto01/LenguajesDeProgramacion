module Hoja2Ejercicios 
            (componer,
            impar, 
            signum',
            signum'',
            bisiesto,
            sumaCuatupla) where

componer :: Int -> Int -> Int
componer x y = max (x `div` y) (x `mod` y)

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

