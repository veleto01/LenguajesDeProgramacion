module Listado3Tema4
        (mezclarEnTernas,
        mezclarEnTernasAux,
        alFinal,
        alFinal',
        takeWhile',
        posicionesElem,
        contiene,
        contiene') where

mezclarEnTernasAux :: [a] -> [b] -> [(a,b,b)] -> [(a,b,b)]
mezclarEnTernasAux [] _ r = r
mezclarEnTernasAux _ [] r = r
mezclarEnTernasAux _  [_] r = r
mezclarEnTernasAux (x:xs) (y:z:zs) r = mezclarEnTernasAux xs zs ((x,y,z):r)

mezclarEnTernas :: [a] -> [b] -> [(a,b,b)]
mezclarEnTernas xs ys = reverse (mezclarEnTernasAux xs ys [])

alFinal :: a -> [a] -> [a]
alFinal x [] = [x]
alFinal n (x:xs) = x:alFinal n xs

alFinal' :: a -> [a] -> [a]
alFinal' n = foldr (\x base -> x:base) [n]

takeWhile' :: Eq a => (a -> Bool) -> [a] -> [a]
takeWhile' f = foldl (\base x -> if f x && notElem x base then base ++ [x] else base) []

posicionesElem :: Eq a => (a, [a]) -> [Int]
posicionesElem (x,xs) =  foldl (\base c -> if xs!!c == x then base ++ [c] else base) [] [0..length xs -1]

contiene :: Eq e => e -> [e] -> Bool
contiene x = foldl (\base n -> x==n||base) False

contiene' :: Eq e => e -> [e] -> Bool
contiene' x = foldr (\n base -> x==n||base) False