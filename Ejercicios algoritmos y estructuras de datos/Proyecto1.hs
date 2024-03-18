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
paraTodo'' (x:xs) t = t x && paraTodo'' xs t