{-# LANGUAGE NPlusKPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use camelCase" #-}


-- Ejercicio 1 Tipos Enumerados

--a) Implementa el tipo Carrera como esta definido arriba

data Carrera = Matematica | Fisica | Computacion | Astronomia

--b) Definı la siguiente funcion, usando pattern matching: titulo :: Carrera -> String
-- que devuelve el nombre completo de la carrera en forma de string. Por ejemplo, para el
-- constructor Matematica, debe devolver ”Licenciatura en Matematica”.

titulo :: Carrera -> String
titulo Matematica    = "Licenciatura en Matemática"
titulo Fisica        = "Licenciatura en Física"
titulo Computacion   = "Licenciatura en Computación"
titulo Astronomia    = "Licenciatura en Astronomía"


--titulo Astronomia --> Licenciatura en Astronomía
--titulo Fisica     --> Licenciatura en Física


-- c) Para escribir m´usica se utiliza la denominada notacion musical, la cual consta de
-- notas (do, re, mi, ...). Ademas, estas notas pueden presentar algun modificador ]
-- (sostenido) o [ (bemol), por ejemplo do], si[, etc. Por ahora nos vamos a olvidar de
-- estos modificadores (llamados alteraciones) y vamos a representar las notas basicas.
-- Definir el tipo NotaBasica con constructores Do, Re, Mi, Fa, Sol, La y Si

data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Ord, Show)

-- d) El sistema de notacion musical anglosajon, tambien conocido como notacion o cifrado
-- americano, relaciona las notas basicas con letras de la A a la G. Este sistema se usa por
-- ejemplo para las tablaturas de guitarra. Programar usando pattern matching la funcion:
-- cifradoAmericano : : NotaBasica −> Char
-- que relaciona las notas Do, Re, Mi, Fa, Sol, La y Si con los caracteres ’C’ , ’D’, ’E’,
-- ’F’, ’G’, ’A’ y ’B’ respectivamente

cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do     = 'C'
cifradoAmericano Re     = 'D'
cifradoAmericano Mi     = 'E'
cifradoAmericano Fa     = 'F'
cifradoAmericano Sol    = 'G'
cifradoAmericano La     = 'A'
cifradoAmericano Si     = 'B'


--cifradoAmericano Sol --> 'G'
--cifradoAmericano La  --> 'A'


-- Ejercicio 2 Clases de tipos

--a)Completar la definicion del tipo NotaBasica para que las expresiones
-- *Main> Do <= Re
-- *Main> Fa `min` Sol
-- sean validas y no generen error. Ayuda: usar deriving con multiples clases

-- Do <= Re     --> True
-- Fa `min` Sol --> Fa


-- Ejercicio 3 Polimorfismo ad hoc  

--a) Definir usando polimorfismo ad hoc la funcion minimoElemento que calcula (de manera
-- recursiva) cual es el menor valor de una lista de tipo [a]. Asegurarse que solo este
-- definida para listas no vacıas

minimoElemento :: (Ord a) => [a] -> a
minimoElemento [x] = x
minimoElemento (x : xs) = min x (minimoElemento xs)

--minimoElemento [2]         --> 2
--minimoElemento [5,2,3,7,8] --> 2


--b)Definir la funcion minimoElemento’ de manera tal que el caso base de la recursion
-- sea el de la lista vacıa. Para ello revisar la clase Bounded.
-- Ayuda: Para probar esta funcion dentro de ghci con listas vacıas, indicar el tipo concreto con tipos de la clase Bounded, por ejemplo: ([1,5,10]::[Int]), ([]::[Bool]),etc.

minimoElemento' :: (Ord a, Bounded a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x : xs) = min x (minimoElemento' xs)

--minimoElemento' ([2,5,6,7,2,1]:: [Int])  --> 1
--minimoElemento' ([]:: [Int])             --> 9223372036854775807

--c) Usar la funcion minimoElemento para determinar la nota mas grave de la melodıa: [Fa, La, Sol, Re, Fa]

instance Bounded NotaBasica where
    minBound :: NotaBasica
    minBound = Do
    maxBound :: NotaBasica
    maxBound = Si

    -- minimoElemento [Fa, La, Sol, Re, Fa]  --> Re
    -- minimoElemento' [Fa, La, Sol, Re, Fa] --> Re


-- Ejercicio 4 Sinonimo de tipos; constructores con parametros. 


--a)
-- Implementa el tipo Deportista y todos sus tipos accesorios (NumCamiseta, Altura,Zona, etc) tal como estan definidos arriba.

-- lista de deportistas para testeo
deportistas :: [Deportista]
deportistas = [Velocista 170, Ciclista Pista, Tenista DosManos Izquierda 180, Futbolista Delantera 10 Derecha 175, Velocista 160, Futbolista Arco 1 Izquierda 175]

type Altura = Int
type NumCamiseta = Int

data Zona = Arco | Defensa | Mediocampo | Delantera deriving (Eq)
data TipoReves = DosManos | UnaMano deriving (Eq)
data Modalidad = Carretera | Pista | Monte | BMX deriving (Eq)
data PiernaHabil = Izquierda | Derecha deriving (Eq)

type ManoHabil = PiernaHabil

data Deportista = Ajedrecista
                |Ciclista Modalidad
                |Velocista Altura
                |Tenista TipoReves ManoHabil Altura
                |Futbolista Zona NumCamiseta PiernaHabil Altura


--b) ¿Cual es el tipo del contructor Ciclista?

--Ciclista :: Modalidad -> Deportista

--El constructor ciclista es del tipo Deportista y necesita un parametro (Modalidad)

--c) Programa la funcion contar_velocistas :: [Deportista] -> Int que dada una lista de deportistas xs, devuelve la cantidad de velocistas que hay dentro de xs

contar_velocistas :: [Deportista] -> Int
contar_velocistas [] = 0
contar_velocistas (x : xs) | esVelocista x = 1 + contar_velocistas xs
                           | otherwise = contar_velocistas xs
                           where esVelocista (Velocista {}) = True
                                 esVelocista _ = False

--contar_velocistas deportistas --> 2

--d)  Programa la funcion contar_futbolistas :: [Deportista] -> Zona -> Int que dada una lista de deportistas xs, y una zona z, devuelve la cantidad de futbolistas incluidos en xs que juegan en la zona z. Programa contar_futbolistas sin usar igualdad, utilizando pattern matching.

contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] _ = 0
contar_futbolistas ((Futbolista zona _ _ _) : xs) z | zona == z = 1 + contar_futbolistas xs z
                                                    |otherwise = contar_futbolistas xs z
contar_futbolistas (x:xs) z = contar_futbolistas xs z

--contar_futbolistas deportistas Arco --> 1


--e)¿La funcion anterior usa filter? Si no es ası, reprogramala para usarla

contar_futbolistas' :: [Deportista] -> Zona -> Int
contar_futbolistas' deportistas z = length (filter (esFutbolistaDeZona z) deportistas)
    where esFutbolistaDeZona z (Futbolista zona _ _ _) = z == zona
          esFutbolistaDeZona _ _ = False

--contar_futbolistas' deportistas Arco  --> 1