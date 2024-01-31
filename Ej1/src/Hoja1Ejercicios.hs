module Hoja1Ejercicios 
            (suma,
            doble,
            sucesor,
            sucesor',
            sucesor'',
            cuadruple,
            finSemana,
            sucesorSumaDobles) where

suma :: Int -> Int -> Int -> Int
suma x y z = x + y + z

doble :: Int -> Int 
doble x = 2 * x

sucesor :: Int -> Int
sucesor x = x + 1

sucesor' :: Int -> Int
sucesor' x = succ x 

sucesor'' :: Int -> Int
sucesor'' = suma 1 0

cuadruple :: Int -> Int
cuadruple = doble.doble 

finSemana :: (String, String)
finSemana = ("Sabado", "Domingo")

sucesorSumaDobles :: Int -> Int -> Int
sucesorSumaDobles x y = sucesor(doble x + doble y)

