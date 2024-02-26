module Listado1Tema5
        (descuentoAbono,
        aprobado,
        calificacion,
        calificacionAlumno,
        dameNota,
        mediaNotas,
        aprobado',
        calificacionAlumno',
        dameNota',
        mediaNotas',
        sumaNotas,
        sumaNotas',
        parteReal,
        sumaComplejos,
        proporcional,
        equivalentes,
        proporcional',
        equivalentes'
        ) where

type Nombre = String
type Edad = Integer
type Persona = (Nombre, Edad)

type Dni = String
type Expediente = Int
type Nota = Float

descuentoAbono :: Persona -> Bool
descuentoAbono (_, e) = (e <= 18 || e>=67)

type Alumno = (Dni, Expediente, Nota) 

aprobado :: Alumno -> Bool
aprobado (_, _ , nota) = nota >= 5.0

calificacionAlumno :: Alumno -> String
calificacionAlumno (_, expedient, nota) = "Expediente:"++show expedient ++ ", Nota Acta:" ++ calificacion nota

dameNota :: Alumno -> Float
dameNota (_, _, nota) = nota

sumaNotas :: [Alumno] -> Float
sumaNotas [] = 0
sumaNotas ((_, _, nota):xs) = nota + sumaNotas xs

mediaNotas :: [Alumno] -> Float
mediaNotas x = sumaNotas x / (fromIntegral (length x))

data Alumno' = Alumno Dni Expediente Nota deriving Show

aprobado' :: Alumno' -> Bool
aprobado' (Alumno _ _ nota) = nota >= 5.0

calificacion :: Float -> String
calificacion x 
    | x  >= v0 && x < v1 = "Suspenso"
    | v1 <= x  && x < v2 = "Aprobado"
    | v2 <= x  && x < v3 = "Notable"
    | v3 <= x  && x < v4 = "Sobresaliente"
    | x == v4            = "Matricula Honor"
    | otherwise          = "Nota no valida"
        where
            v0 = 0
            v1 = 5
            v2 = 7
            v3 = 9
            v4 = 10

calificacionAlumno' :: Alumno' -> String
calificacionAlumno' (Alumno _ expedient nota) = "Expediente:"++show expedient ++ ", Nota Acta:" ++ calificacion nota

dameNota' :: Alumno' -> Float
dameNota' (Alumno _ _ nota) = nota

sumaNotas' :: [Alumno'] -> Float
sumaNotas' [] = 0
sumaNotas' ((Alumno _ _ nota):xs) = nota + sumaNotas' xs

mediaNotas' :: [Alumno'] -> Float
mediaNotas' x = sumaNotas' x / (fromIntegral (length x))

data Complejo = Com Float Float deriving Show

parteReal :: Complejo -> Float
parteReal (Com r _) = r

sumaComplejos :: Complejo -> Complejo -> Complejo
sumaComplejos (Com r1 i1) (Com r2 i2) = (Com (r1+r2) (i1+i2))

type Racional = (Int, Int)

proporcional :: Racional -> Racional -> Bool
proporcional (a,b) (c,d) = a*d == b*c

equivalentes :: Racional -> [Racional] -> [Racional]
equivalentes nr rs = [r | r<-rs, proporcional nr r]

data Racional' = R {numerador :: Int, denominador :: Int} deriving Show

proporcional' :: Racional' -> Racional' -> Bool
proporcional' r1 r2 = numerador r1 * denominador r2 == denominador r1 * numerador r2

equivalentes' :: Racional' -> [Racional'] -> [Racional']
equivalentes' nr rs = [r | r <- rs, proporcional' nr r]