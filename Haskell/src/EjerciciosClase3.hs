module EjerciciosClase3 
        (aprobado,
        aprobado',
        lengthMatriz,
        m,
        diasSemanaLaborables,
        diasSemanaLaborables',
        expresiones,
        ej3,
        isHoja,
        numHojas,
        posicion,
        posicionAux
        ) where
    
type Dni = String
type Expediente = Int
type Nota = Float
type Alumno = (Dni, Expediente, Nota) 

aprobado :: Alumno -> Bool
aprobado (_, _, n) = n >= 5.0


data Alumno' = A Dni Expediente Nota deriving Show

aprobado' :: Alumno' -> Bool
aprobado' (A _ _ nota) = nota >= 5.0


data Matriz = M {matriz :: [[Int]]} deriving Show

m :: Matriz
m = M [[1,2,3],[4,5,6],[7,8,9]]

lengthMatriz :: Matriz -> (Int, Int)
lengthMatriz (M []) = (0,0)
lengthMatriz (M filas) = (length filas, length(head filas))

data Dia = D String deriving Show

data Dias = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

diasSemanaLaborables :: [Dia]
diasSemanaLaborables = [(D "lunes"), (D "martes"), (D "miercoles"), (D "jueves"), (D "viernes")]

diasSemanaLaborables' :: [Dias]
diasSemanaLaborables' = [Lunes, Martes, Miercoles, Jueves, Viernes]

data Natural = Cero | Suc Natural deriving Show

data Expr = Valor Integer 
        |Expr :+: Expr
        |Expr :-: Expr
        |Expr :*: Expr deriving Show

ej3 :: Expr
ej3 =  Valor 5 :+: Valor 3 :*: Valor 10

expresiones :: Expr -> Int
expresiones (Valor _) = 0
expresiones (e1 :+: e2) = 1 + (expresiones e1) + expresiones e2 
expresiones (e1 :-: e2) = 1 + (expresiones e1) + expresiones e2 
expresiones (e1 :*: e2) = 1 + (expresiones e1) + expresiones e2 

data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

isHoja :: Arbol a -> Bool
isHoja (Rama AV _ AV) = True
isHoja _ = False

numHojas :: Arbol a -> Int
numHojas AV = 0
numHojas (Rama AV _ AV) = 1
numHojas (Rama izq _ der) = numHojas izq + numHojas der

posicion :: [Int] -> Int -> Maybe Int
posicion lista n = posicionAux lista n 0

posicionAux :: [Int] -> Int -> Int -> Maybe Int
posicionAux [] _ _= Nothing
posicionAux (x:xs) n ac 
        | x == n = Just ac
        | otherwise = posicionAux xs n (ac+1)