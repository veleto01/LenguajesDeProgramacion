module Listado2Tema5 
        (todosIguales,
        todosIgualesAux,
        primeraIgual,
        subconjunto,
        sumaNaturales,
        restaNaturales,
        multiplicarNaturales,
        tres,
        dos,
        valorExpr,
        constantesExpr,
        operadoresExpr,
        elemArbol,
        elemArbol',
        eliminarDuplicados,
        eliminarDuplicadosAux,
        primerPropiedad
        ) where

todosIguales :: Eq a => [a] -> Bool
todosIguales [] = True
todosIguales lista = todosIgualesAux lista (head lista)

todosIgualesAux :: Eq a => [a] -> a -> Bool
todosIgualesAux [] _ = True
todosIgualesAux (x:xs) ele
    | x == ele = True && todosIgualesAux xs ele
    | otherwise = False

primeraIgual :: Eq a => [a] -> [a] -> Bool
primeraIgual _ [] = True
primeraIgual [] _ = True
primeraIgual (x:xs) (y:ys)
    | x == y = True && primeraIgual xs ys
    | otherwise = False


subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto _ [] = False
subconjunto l1 l2@(_:xs)
    | length l1 > length l2 = False
    | primeraIgual l1 (take (length l1) l2) = True
    | otherwise = subconjunto l1 xs

data Natural = Cero | Suc Natural deriving Show

tres :: Natural
tres = Suc(Suc(Suc Cero))

dos :: Natural
dos = Suc(Suc Cero)

sumaNaturales :: Natural -> Natural -> Natural
sumaNaturales Cero Cero = Cero
sumaNaturales (Suc x) Cero = Suc x
sumaNaturales Cero (Suc y) = Suc y
sumaNaturales (Suc x) (Suc y) = Suc(Suc(sumaNaturales x y))

--Suponiendo que siempre de un resultado positivo porque no podemos expresar uno negativo con Naturales
--por lo tanto el primer parametro es siempre mayor que el segundo
restaNaturales :: Natural -> Natural -> Natural
restaNaturales Cero Cero = Cero
restaNaturales (Suc x) Cero = Suc x
restaNaturales Cero (Suc y) = Suc y
restaNaturales (Suc x) (Suc y) = restaNaturales x y 

multiplicarNaturales :: Natural -> Natural -> Natural
multiplicarNaturales Cero Cero = Cero
multiplicarNaturales (Suc _) Cero = Cero
multiplicarNaturales Cero (Suc _) = Cero
multiplicarNaturales (Suc x) y@(Suc _) = sumaNaturales y (multiplicarNaturales x y)

data Expr = Valor Integer 
        |Expr :+: Expr
        |Expr :-: Expr
        |Expr :*: Expr deriving Show
    
valorExpr :: Expr -> Integer
valorExpr (Valor x) = x
valorExpr (e1 :*: e2) = valorExpr e1 * valorExpr e2
valorExpr (e1 :-: e2) = valorExpr e1 - valorExpr e2 
valorExpr (e1 :+: e2) = valorExpr e1 + valorExpr e2

constantesExpr :: Expr -> Int
constantesExpr (Valor _) = 1
constantesExpr (e1 :+: e2) = constantesExpr e1 + constantesExpr e2 
constantesExpr (e1 :-: e2) = constantesExpr e1 + constantesExpr e2 
constantesExpr (e1 :*: e2) = constantesExpr e1 + constantesExpr e2 

operadoresExpr :: Expr -> Int
operadoresExpr (Valor _) = 0
operadoresExpr (e1 :+: e2) = 1 + operadoresExpr e1 + operadoresExpr e2 
operadoresExpr (e1 :-: e2) = 1 + operadoresExpr e1 + operadoresExpr e2 
operadoresExpr (e1 :*: e2) = 1 + operadoresExpr e1 + operadoresExpr e2 

type Nombre = String
type Edad = Int
data Persona = P Nombre Edad deriving (Show, Eq, Ord)

data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving (Show, Eq, Ord)

elemArbol :: Eq a => Arbol a -> a -> Bool
elemArbol AV _ = False
elemArbol (Rama AV r AV) x = r == x
elemArbol (Rama izq r der) x
    | r==x = True
    | otherwise = elemArbol izq x || elemArbol der x

--Asumiendo que es ABB
elemArbol' :: Ord a => Arbol a -> a -> Bool
elemArbol' AV _ = False
elemArbol' (Rama AV r AV) x = r == x
elemArbol' (Rama izq r der) x
    | x < r = elemArbol' izq x
    | x > r = elemArbol' der x
    | otherwise = True

eliminarDuplicados :: Eq a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados lista = eliminarDuplicadosAux (drop 1 lista) (head lista)

eliminarDuplicadosAux :: Eq a => [a] -> a -> [a]
eliminarDuplicadosAux [] _ = []
eliminarDuplicadosAux [x] n
    | x == n = [n]
    | otherwise = [n,x]
eliminarDuplicadosAux (x:xs) n 
    | x == n = eliminarDuplicadosAux xs x
    | otherwise = [n]++eliminarDuplicadosAux xs x

primerPropiedad :: (a -> Bool) -> [a] -> Maybe a
primerPropiedad _ [] = Nothing
primerPropiedad f (x:xs)
    | f x = Just x
    | otherwise = primerPropiedad f xs