-- Ejercicio 4: ¿Qué hacen las siguientes funciones?
-- Ayuda: Evaluá las funciones para algunos valores.

-- a)           contar: [Int] → Int
-- contar.[ ] ≐ 0
-- contar.(x ▹ xs) ≐ 1 + contar.xs
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}



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


