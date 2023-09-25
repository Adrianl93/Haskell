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

--Ejercicio 8
--Definí la función esMultiploDe : Num→ Num→ Bool , que devuelve True si el segundo es múltiplo del primero. Ejemplo: esMultiploDe 3 12 = True.

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod y x == 0

-- Ejercicio 9
-- Definí la función esBisiesto: Num→ Bool , que indica si un año es bisiesto. Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100.

esBisiesto :: Int -> Bool
esBisiesto x = (mod x 400 == 0 || mod x 4 == 0) && (mod x 100 /= 0)


--Ejercicio 10
--Definí la función dispersion : Num→ Num→ Num→ Num, que toma los tres valores y devuelve la diferencia entre el más alto y el más bajo. Ayuda: extender max y min a tres argumentos, usando las versiones de dos elementos. De esa forma se puede definir dispersión sin escribir ninguna guarda (las guardas están en max y min, que estamos usando).

max3 :: Int -> Int -> Int -> Int
max3 x y z = max z (max x y) 

min3 :: Int -> Int -> Int -> Int
min3 x y z = min z (min x y) 

dispersion :: Int -> Int -> Int -> Int
dispersion x y z =max3 x y z - min3 x y z


--Ejercicio 11: Definí la función celsiusToFahr : Num→ Num, pasa una temperatura en grados Celsius a grados Fahrenheit. Para realizar la conversión hay que multiplicar por 1.8 y sumar 32.

celsiusToFahr :: Float -> Float
celsiusToFahr x = (x * 1.8) + 32

-- Ejercicio 12: Definí la función fahrToCelsius : Num→ Num, la inversa de la anterior. Para realizar la conversión hay que primero restar 32 y después dividir por 1.8.

fahrToCelsius :: Float -> Float
fahrToCelsius x = (x -32) / 1.8

-- Ejercicio 13: Definí la función haceFrioF : Num→ Bool , indica si una temperatura expresada en grados Fahrenheit es fría. Decimos que hace frío si la temperatura es menor a 8 grados Celsius.

haceFrioF :: Float -> Bool
haceFrioF x = fahrToCelsius x < 8