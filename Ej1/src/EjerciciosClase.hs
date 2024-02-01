module EjerciciosClase
            (hayInterseccion,
            mayorDeTres) where

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
    | y >= x && y >= z = y
    | otherwise = z
    
    