{-# LANGUAGE NPlusKPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}


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

multiplicaPrimos :: [Int] -> Int 
multiplicaPrimos xs = productoria' (filter esPrimo xs) (*1)

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
--todosFib [34, 55, 88] --> True