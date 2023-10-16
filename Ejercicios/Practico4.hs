-- Ejercicio 4: ¿Qué hacen las siguientes funciones?
-- Ayuda: Evaluá las funciones para algunos valores.

-- a)           contar: [Int] → Int
-- contar.[ ] ≐ 0
-- contar.(x ▹ xs) ≐ 1 + contar.xs
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use odd" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use even" #-}



contar :: [Int] -> Int
contar []        = 0
contar (x : xs) = 1 + contar xs

-- b)           f: [Int] → Bool
-- f.[ ] ≐ False
-- f.(x ▹ xs) ≐ x = 5 ∨ f.xs

f :: [Int] -> Bool
f []        = False
f (x : xs)  = x  == 5 || f xs


-- Ejercicio 5:  Una función fold es aquella que dada una lista devuelve un valor resultante de combinar los elementos de la lista. Por ejemplo: sumar: [Int] → Int devuelve la sumatoria de los elementos de la lista.

-- Definí recursivamente las siguientes funciones fold.
-- a) sumar: [Int] → Int, que dada una lista de enteros devuelve la suma de todos sus elementos.
sumar :: [Int] -> Int
sumar []         = 0
sumar (x : xs)   = x + sumar xs
-- b) prod: [Int] → Int, que dada una lista de enteros devuelve el producto de todos sus elementos.
prod :: [Int] -> Int
prod []       = 1
prod (x : xs) = x * prod xs
-- c) card: [Int] → Int, que dada una lista devuelve la cantidad de elementos de la lista.
card :: [Int] -> Int
card []       = 0
card (x : xs) = 1 + card xs
-- d) todosMenores10: [Int] → Bool, que dada una lista devuelve True si ésta consiste sólo de números menores que 10 y False en caso contrario. 
todosMenores10 :: [Int] -> Bool
todosMenores10 []       = True
todosMenores10 (x : xs) = x < 10 && todosMenores10 xs
-- e) hay0: [Int] → Bool, que dada una lista decide si existe algún 0 en ella.
hay0 :: [Int] -> Bool
hay0 []         = False
hay0 (x : xs) = x == 0 || hay0 xs

-- Ejercicio 6: Una función map es aquella que dada una lista devuelve otra lista cuyos elementos son los que se obtienen de aplicar una función a cada elemento de la primera en el mismo orden y con las mismas repeticiones (si las hubiere). Por ejemplo: duplica : [Int] → [Int] devuelve cada elemento de la lista multiplicado por 2.
-- Definí recursivamente las siguientes funciones map.

--a) sumar1: [Int] → [Int], que dada una lista de enteros le suma uno a cada uno de sus elementos. 
-- Por ejemplo: sumar1.[3, 0, −2] = [4, 1, −1]
sumar1 :: [Int] -> [Int]
sumar1 [] = []
sumar1 (x:xs) = (x+ 1) : sumar1 xs

-- b) duplica: [Int] → [Int], que dada una lista de enteros duplica cada uno de sus elementos.
-- Por ejemplo: duplica.[3, 0, −2] = [6, 0, −4]

duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (x * 2) : duplica xs

-- c) multiplica: Int → [Int] → [Int], que dado un número n y una lista, multiplica cada uno de los elementos por n.
-- Por ejemplo: multiplica.3.[3, 0, −2] = [9, 0, −6]
multiplica :: Int -> [Int] -> [Int]
multiplica b [] = []
multiplica b (x:xs) = (b * x) : multiplica b xs



-- Ejercicio 7: Una función filter es aquella que dada una lista devuelve otra lista cuyos elementos son los elementos de la primera que cumplan una determinada condición, en el mismo orden y con las mismas repeticiones (si las hubiere). Por ejemplo: soloPares : [Int] → [Int] devuelve aquellos elementos de la lista que son pares.
-- Definí recursivamente las siguientes funciones filter.


-- a) soloPares: [Int] → [Int], que dada una lista de enteros xs devuelve una lista sólo con los números pares contenidos en xs, en el mismo orden y con las mismas repeticiones (si las hubiera).
-- Por ejemplo: soloPares.[3, 0, −2, 12] = [0, −2, 12]

soloPares :: [Int] -> [Int ]
soloPares [] = []
soloPares (x:xs) | ( mod x 2 == 0) = x : soloPares xs
                 | (mod x 2 /= 0) = soloPares xs


-- b) mayoresQue10: [Int] → [Int], que dada una lista de enteros xs devuelve una lista sólo con los números mayores que 10 contenidos en xs,
-- Por ejemplo: mayoresQue10.[3, 0, −2, 12] = [12]
mayoresQue10 :: [Int] -> [Int]
mayoresQue10 [] = []
mayoresQue10 (x:xs) | x > 10 = x : mayoresQue10 xs
                    | x <= 10 = mayoresQue10 xs

-- c) mayoresQue: Int → [Int] → [Int], que dado un entero n y una lista de enteros xs devuelve una lista sólo con los números mayores que n contenidos en xs,
-- Por ejemplo: mayoresQue.2.[3, 0, −2, 12] = [3, 12]

mayoresQue :: Int -> [Int] -> [Int]
mayoresQue b [] = []
mayoresQue b (x:xs) | x > b = x : mayoresQue b xs
                    | x <= b = mayoresQue b xs


-- Ejercicio 8: Definí recursivamente los operadores básicos de listas: #, ! , ◃, ↑, ↓, ⧺ . Para los operadores ↑, ↓ y !, deberás hacer recursión en ambos parámetros, en el parámetro lista y en el parámetro numérico

cardinal :: [a] -> Int
cardinal [] = 0
cardinal (x:xs) = 1 + cardinal xs

indice :: [a] -> Int -> a
indice (x:xs) b | b > 0 = indice xs (b-1) 
                | b == 0 = x

pegarIzq :: [a] -> a -> [a]
pegarIzq [] y = [y]
pegarIzq (x:xs) y = y: x : xs

pegarDer :: [a] -> a -> [a]
pegarDer [] y = [y]
pegarDer (x:xs) y = (x : xs) ++ [y]

tomar :: Int -> [a] -> [a]
tomar y [] = []
tomar y (x : xs) | y > 0 = x : tomar (y-1) xs 
                 | y == 0 = []

tirar :: Int -> [a] -> [a]
tirar y [] = []
tirar y (x : xs) | y < length (x : xs) =  tirar y xs 
                 | y <= length (x : xs) = (x : xs)

concatenar :: [a] -> [a] -> [a]
concatenar [] ys = ys
concatenar (x:xs) ys = x : concatenar xs ys


-- Funciones recursivas de sobre los números naturales
-- Ejercicio 9: ¿Qué hacen las siguientes funciones?
-- Ayuda: Evaluá las funciones para algunos valores.


-- a)           h: Nat → Nat
-- h.0         ≐ 0
-- h.(n+1) ≐ 2 + h.n

h :: Int -> Int
h 0 = 0
h n = 2 + h (n-1)

-- duplica cualquier n

-- b)           g: Nat → Bool
-- g.0         ≐ True
-- g.1         ≐ False
-- g.(n+2) ≐ g.n

g :: Int -> Bool
g 0 = True
g 1 = False
g n = g (n-2)

-- si n es par devuelve False, si n es Impar devuelve True

-- Ejercicio 10: Definí recursivamente las siguientes funciones de Naturales en Naturales

-- a) la función acumular: Nat → Nat, que dado un Natural n devuelve la suma de todos los naturales menores o iguales a n.
acumular :: Int -> Int
acumular 0 = 0
acumular x = x + (acumular (x-1))

-- b) la función factorial: Nat → Nat, que dado un Natural n devuelve el factorial de n.

factorial :: Int -> Int
factorial 1 = 1
factorial x = x * (factorial (x-1))
-- c) la función sumaCuadrados: Nat → Nat, que dado un Natural n devuelve la suma de todos los naturales menores o iguales a n elevados al cuadrado.

sumaCuadrados :: Int -> Int
sumaCuadrados 1  = 1  
sumaCuadrados x  = (x * x) + sumaCuadrados (x-1)


-- d) la función repetir: Nat → Nat → Nat, que dado dos naturales n y m, suma n veces el número m.

repetir :: Int -> Int -> Int
repetir 1 m = m
repetir n m = m + repetir (n-1) m

