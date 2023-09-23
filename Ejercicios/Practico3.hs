-- Ejercicio 2
-- Escribí una función que dados dos valores, calcule su promedio. Luego definila en Haskell 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use even" #-}
promedio :: Float -> Float -> Float
promedio x y = (x + y)/2


-- Ejercicio 7
--Definí la función signo : Int → Int, que dado un entero retorna su signo, de la siguiente forma: retorna 1 si x es positivo, -1 si es negativo y 0 en cualquier otro caso.


signo :: Int -> Int
signo x  | x > 0 = 1
         | x<0 = -1
         | x==0 = 0
          

-- Ejercicio 7 A
-- entre0y9 : Int → Bool, que dado un entero devuelve True si el entero se encuentra entre 0 y 9.
entre0y9 :: Int -> Bool
entre0y9 x = x <= 9 && x >= 0 

-- Ejercicio 7 B
-- rangoPrecio : Int → String, que dado un número que representa el precio de una computadora, retorne “muy barato” si el precio es menor a 2000, “demasiado caro” si el precio es mayor que 5000, “hay que verlo bien” si el precio está entre 2000 y 5000, y “esto no puede ser!” si el precio es negativo.


rangoPrecio :: Int -> String
rangoPrecio x | x < 2000 && x > 0 = "Muy barato"
              | x >= 5000 = "Demasiado caro"
              | x >= 2000 && x < 5000 = "Hay que verlo bien"
              | x < 0 = "Esto no puede ser"

-- Ejercicio 7 C

-- absoluto : Int → Int, que dado un entero retorne su valor absoluto.

absoluto :: Int -> Int
absoluto x | x > 0 = x
           | x < 0 = -x


-- Ejercicio 7 D
-- esMultiplo2 : Int → Bool, que dado un entero n devuelve True si n es múltiplo de 2.

esMultiplo2 :: Int -> Bool
esMultiplo2 x = mod x 2  == 0


