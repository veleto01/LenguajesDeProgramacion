module Hoja2Ejercicios 
            (componer,
            impar, 
            signum',
            signum'',
            bisiesto,
            sumaCuatupla) where

componer :: Int -> Int -> Int
componer x y = max (x `div` y) (x `mod` y)
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

