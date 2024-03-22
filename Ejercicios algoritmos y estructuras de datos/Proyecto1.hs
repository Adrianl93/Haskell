{-# LANGUAGE NPlusKPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import System.Win32 (COORD(xPos))
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant bracket" #-}


-- ejercicio 1
-- a)
esCero :: Int -> Bool
esCero x = x == 0

-- esCero 0 -> True
-- esCero 2 -> False

--b)
esPositivo :: Int -> Bool
esPositivo x = x > 0

--esPositivo 1 --> True
--esPositivo (-9) --> False
--esPositivo 0 --> False

--c)
esVocal :: Char -> Bool
esVocal x = x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u'

-- esVocal 'a' --> True
-- esVocal 'b' --> False

--d)
valorAbsoluto :: Int -> Int
valorAbsoluto x | x > 0 = x
                | x <=0 = x + x * (-2)


-- valorAbsoluto 3 --> 3
-- valorAbsoluto (-3) --> 3


-- ejercicio 2

--a)
paraTodo :: [Bool] -> Bool
paraTodo [] = True
paraTodo (x : xs) = x && paraTodo xs

--paraTodo [True, True, True] --> True
--paraTodo [True, False, True] --> False


--b)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x : xs) = x + sumatoria xs

--sumatoria [2,4,4] --> 10
--sumatoria [(-9),2,3] --> -4

--c) 
productoria :: [Int] -> Int
productoria [] = 1
productoria (x : xs) = x * productoria xs

--productoria [2,2,2] --> 8
--productoria [2, 4, (-2)] --> -16

--d) 
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

--factorial 4 --> 24
--factorial 5 --> 120


--e)
promedio :: [Int] -> Int
promedio (x : xs) = (sumatoria (x : xs)) `div` length (x : xs)

--promedio [2, 6, 8] --> 5
--promedio [2, 4, 8] --> 4



-- ejercicio 3

pertenece :: Int -> [Int] -> Bool
pertenece y [] = False
pertenece y (x : xs) = x == y || pertenece y xs


--pertenece 3 [2, 5, 7, 8, 9, 3] --> True
--pertenece 4 [2, 5, 7, 8, 3] --> False


-- ejercicio 4

--a) 
paraTodo' :: [a] -> (a -> Bool) -> Bool
paraTodo' [] t = True
paraTodo' (x : xs) t = t x && paraTodo' xs t

--paraTodo' [2, 3, 4, 5] (> 0) -> True
--paraTodo' [1, 4, 5, 7] (==0) -> False

--b)
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x : xs) t = t x || existe' xs t


--existe' [1,2,3] (==2) -> True
--existe' [(-2),2,3] (< 0) -> True

--c) 
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x : xs) t = t x + sumatoria' xs t

--sumatoria' [2, 3, 4] (*2) --> 18
--sumatoria' [4, 8 ,7] (+1) --> 22

--d)
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria' (x : xs) t = t x * productoria' xs t

--productoria' [1,2,3] (*2) -> 48
--productoria' [1,2,3] (+1) -> 24



-- ejercicio 5

paraTodo'' :: [a] -> (a -> Bool) -> Bool
paraTodo'' xs t = paraTodo' xs t

--paraTodo'' [2, 4, 5] (>0) --> True
--paraTodo'' [2, 4, 5] (>4) --> False


--ejercicio 6
--a)
todosPares :: [Int] -> Bool
todosPares xs = paraTodo' xs even

--todosPares [2, 4, 6] --> True
--todosPares [2, 4, 5] --> False

--b)
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo y xs = existe' xs esMultiplo
                   where esMultiplo x = mod x y == 0

--hayMultiplo 2 [2, 4, 6, 8] --> True
--hayMultiplo 3 [2, 4, 6, 8] --> True
--hayMultiplo 5 [2, 4, 6, 8] --> False


--c)

sumaCuadrados :: Int -> Int 
sumaCuadrados n = sumatoria' [0..n] (^2) 

-- sumaCuadrados 4 --> 30
-- sumaCuadrados 2 --> 55

--d)

existeDivisor :: Int -> [Int] -> Bool
existeDivisor n xs = existe' xs esDivisor
                    where esDivisor x = mod n x == 0


--existeDivisor 8 [2, 4, 8] --> True
--existeDivisor 2 [3, 4, 8] --> False


--e)
-- Utilizando la funcion del apartado anterior, definı la funci ́on esPrimo:: Int -> Bool,
-- que dado un entero n, devuelve True si y solo si n es primo.
-- Ayuda: En Haskell se puede escribir la lista que contiene el rango de n ́umeros entre n
-- y m como [n..m].

esPrimo :: Int -> Bool
esPrimo n = not (existeDivisor n [2..n-1])

-- esPrimo 5 --> True
-- esPrimo 7 --> True
-- esPrimo 8 --> True


-- f )

factorial' :: Int -> Int
factorial' n = productoria' [1..n] (*1)

--factorial' 4 --> 24
--factorial' 3 --> 6

--g)

filtro :: [a] -> (a -> Bool) ->[a]
filtro [] f = []
filtro (x : xs) f | f x  = x : filtro xs f
                  | not (f x)  = filtro xs f 

multiplicaPrimos :: [Int] -> Int 
multiplicaPrimos xs = productoria' (filtro xs esPrimo) (*1)


                  

--multiplicaPrimos [2, 3, 5, 8] --> 30
--multiplicaPrimos [2,3,8,12] --> 6

-- h)
-- Programar la funcion esFib :: Int -> Bool, que dado un entero n, devuelve True
-- si y s´olo si n est´a en la sucesi´on de Fibonacci.
-- Ayuda: Realizar una funcion auxiliar fib :: Int -> Int que dado un n devuelva el enesimo elemento de la sucesion.

fib :: Int -> Int
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n - 1) + fib (n - 2)


esFib :: Int -> Bool
esFib n = existe' (takeWhile (<= n) [fib i | i <- [0..]]) (== n)


-- esFib 144 --> True
-- esFib 10 --> False

--i)
-- Utilizando la funci ́on del apartado anterior, defin ́ı la funci ́on todosFib :: [Int] -> Bool
-- que dada una lista xs de enteros, devuelva si todos los elementos de la lista pertenecen
-- (o no) a la sucesi ́on de Fibonacci.

todosFib :: [Int] -> Bool
todosFib xs = paraTodo' xs esFib  

--todosFib [34, 55, 89] --> True
--todosFib [34, 55, 88] --> False


--Ejercicio 7

--Indagá en Hoogle sobre las funciones map y filter. También podes consultar su tipo en ghci con el comando :t.

--a) ¿Qué hacen estas funciones?

--La función Map toma una lista de elementos de tipo a y una funcion cualquiera y aplica dicha funcion sobre todos los elementos de la lista y los vuelve a unir para formar una lista.

--La función Filter toma una lista de elementos y una funcion que devuelve un booleano,luego aplica esta funcion a cada elemento de la lista y conserva unicamente los elementos que devuelven True al aplicar la función 


-- b) ¿A qué equivale la expresión map succ [1, -4, 6, 2, -8], donde succ n = n+1?
-- La expresion aplica map a una lista de numeros y la funcion succ, que devuelve el numero siguiente a n, de esta manera la expresion devuelve una lista con el numero siguiente a cada uno de los numeros enviados, en este caso : [2, 3, 7, 3, 7]

--c)¿Y la expresión filter esPositivo [1, -4, 6, 2, -8]?

--La expresión utiliza filter en una lista de numeros junto con la función esPositivo, por lo cual deberia devolver una lista donde solo queden elementos que satisfacen la propiedad de ser números iguales o mayores a 0, en este caso : [1, 6, 2]


--Ejercicio 8. 

--a) Definila usando recursión.
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x: xs) = x * 2 : duplica xs


--duplica [2, 4 , 6] --> [4, 8, 12]
--duplica [5, 10, 11] --> [10, 20, 22]

--b) Definila utilizando la función map.
duplica' :: [Int] -> [Int]
duplica' xs = map (*2) xs


--duplica' [6, 7, 9] --> [12, 14, 18]
--duplica' [9, 11, 33] --> [18, 28 , 32]

--Programá una función que dada una lista de números xs, calcula una lista que tiene como
--elementos aquellos números de xs que son primos.

--a) Definila usando recursión.
soloPrimos :: [Int] -> [Int]
soloPrimos [] = []
soloPrimos (x : xs) | esPrimo x  = x : soloPrimos xs
                    |otherwise = soloPrimos xs  

--soloPrimos [2,5,7,9,11,18] --> [2, 5, 7, 11]
--soloPrimos [2,5,7,9,11,1,17,20,22] --> [2, 5, 7, 11, 1, 17]

--b) Definila utilizando la función filter.

soloPrimos' :: [Int] -> [Int]
soloPrimos' xs = filter esPrimo xs

--soloPrimos' [2,5,7,9,11,18] --> [2, 5, 7, 11]
--soloPrimos' [2,5,7,9,11,1,17,20,22] --> [2, 5, 7, 11, 1, 17]

--c) Revisá tu definición del ejercicio 6g. ¿Se puede mejorar?



-- Anteriormente habia creado una funcion auxiliar que realizaba el mismo trabajo que Filter 
multiplicaPrimos' :: [Int] -> Int
multiplicaPrimos' xs = productoria' (filter esPrimo xs) (*1)


--multiplicaPrimos' [2,5,6,7] --> 70
--multiplicaPrimos' [2,5,6,7,11,4] --> 770

--Ejercicio 10



--a) Programá primIgualesA por recursión.


primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA x [] = []
primIgualesA x (y : z : ys) | (x == y && x == z )= y : z : primIgualesA x ys
                        | otherwise = []

--primIgualesA 1 [1, 1, 1, 1, 2, 3, 4, 5, 5, 5, 5] --> [1, 1, 1, 1]
--primIgualesA 1 [1, 2] --> []
--primIgualesA 'a' "aaabbbjashaks" -->  "aaa"


--b)  Programá nuevamente la función utilizando takeWhile

primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' x = takeWhile (== x)  

--primIgualesA' 2 [2, 2, 2, 3, 4, 5, 5, 5, 5, 5] --> [2, 2, 2]
--primIgualesA' 'f' "fff000" --> "fff"

-- Ejercicio 11

--a) Program´a primIguales por recursion
primIguales ::Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x : y : ys) | x == y = x : primIguales (y : ys)
                         | x /= y = [x]


-- primIguales [1, 2, 3] --> [1]
-- primIguales [1, 1, 1, 2, 3] --> [1, 1, 1]
-- primIguales "aaaaalo" --> "aaaa"

--b)


-- b) Usa cualquier version de primIgualesA para programar primIguales. Esta permitido
-- dividir en casos, pero no usar recursion.


primIguales' :: Eq a => [a] -> [a]
primIguales' (x:xs) = primIgualesA' x xs

--primIguales [2, 3, 4, 5] --> [2]
--primIguales [2, 2, 2, 4, 5, 5, 5] --> [2, 2, 2]
--primIguales "aaaabbb" --> "aaaa"