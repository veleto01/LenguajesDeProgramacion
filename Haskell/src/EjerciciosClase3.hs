module EjerciciosClase3 
        (aprobado,
        aprobado',
        lengthMatriz,
        m,
        diasSemanaLaborables,
        diasSemanaLaborables',
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