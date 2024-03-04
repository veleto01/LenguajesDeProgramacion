module EjerciciosClase4 
        (arbolesIguales,
        menorListaAux,
        menorLista,
        menorLista',
        buscarPareja
        ) where
    
data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

arbolesIguales :: Eq a => Arbol a -> Arbol a -> Bool
arbolesIguales a1 a2 = a1 == a2 

instance (Eq a) => Eq (Arbol a) where
    AV == AV = True
    AV == _ = False
    _ == AV = False
    (Rama izq1 r1 der1) == (Rama izq2 r2 der2) = (izq1 == izq2) && (r1 == r2) && (der1 == der2)

menorListaAux :: (Ord a) => [a] -> a -> Maybe a
menorListaAux [] r = Just r
menorListaAux (x:xs) mini 
    | x <= mini = menorListaAux xs x
    | otherwise = menorListaAux xs mini

menorLista :: (Ord a) => [a] -> Maybe a
menorLista [] = Nothing
menorLista list = menorListaAux list (head list)

menorLista' :: (Ord a) => [a] -> Maybe a
menorLista' [] = Nothing
menorLista' (x:xs) = foldr (\y base -> if (Just y) < base then (Just y) else base) (Just x) xs

buscarPareja :: (Eq a) => a -> [(a,b)] -> Maybe b
buscarPareja _ [] = Nothing
buscarPareja prim ((e1,e2):xs)
    | prim == e1 = Just e2
    | otherwise = buscarPareja prim xs