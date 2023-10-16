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

-- Ejercicio 12:  Utilizando la multiplicación, definir la función potencia, que dado dos naturales b y p devuelve b^p .

potencia :: Int -> Int -> Int
potencia b 1 = b
potencia b p = b *  potencia b (p-1) 


-- Problemas
-- Los siguientes problemas son un poco más complejos que los que se vieron, especialmente porque se parecen más a los problemas a los que nos enfrentamos en la vida real. Se resuelven desarrollando programas funcionales; es decir, se pueden plantear como la búsqueda de un resultado a partir de ciertos datos (argumentos). Para ello, será necesario en primer lugar descubrir los tipos de los argumentos y del resultado que necesitamos. Luego combinaremos la mayoría de las técnicas estudiadas hasta ahora: modularización (dividir un problema complejo en varias tareas intermedias), análisis por casos, e identificar qué clase de funciones de lista (cuando corresponda) son las que necesitamos: map, filter o bien fold.

-- Ejercicio 16: Definí funciones por recursión para cada una de las siguientes descripciones. Luego, implementarlas en Haskell.
-- a) listasIguales : [A] → [A] → Bool, que determina si dos listas son iguales, es decir, contienen los mismos elementos en las mismas posiciones respectivamente.
-- Por ejemplo: 	listasIguales.[1, 2, 3].[1, 2, 3] = True, 
-- listasIguales.[1, 2, 3, 4].[1, 3, 2, 4] = False

listasIguales :: Eq a => [a] -> [a] -> Bool
listasIguales [] [] = True
listasIguales x [] = False
listasIguales [] x = False
listasIguales (x:xs) (y:ys) = (x == y) && (listasIguales xs ys)

-- b) mejorNota : [(String, Int, Int, Int)] → [(String, Int)], que selecciona la nota más alta de cada alumno. 
-- Por ejemplo: mejorNota.[(“Matias”,7,7,8),(“Juan”,10,6,9),(“Lucas”,2,10,10)] =
-- [(“Matias”,8),(“Juan”,10),(“Lucas”,10)]

mejorNota :: [(String, Int, Int, Int)] -> [(String, Int)]
mejorNota [] = []
mejorNota ((nombre,x,y,z):xs) = (nombre, max x (max y z)) : mejorNota xs

-- c) incPrim : [(Int, Int)] → [(Int, Int)], que dada una lista de pares de enteros, le suma 1 al primer número de cada par.
-- Por ejemplo: 	incPrim.[(20, 5), (50, 9)] = [(21, 5), (51, 9)] 
-- incPrim.[(4, 11), (3, 0)] = [(5, 11), (4, 0)]

incPrim :: [(Int,Int)] -> [(Int,Int)]
incPrim [] = []
incPrim ((x,y):xs) = ((x+1), y) : incPrim xs

-- d) expandir : String → String, pone espacios entre cada letra de una palabra.
-- Por ejemplo: expandir."hola" = "h o l a" (¡sin espacio al final!).
expandir :: String -> String
expandir [] = []
expandir [x] = [x]
expandir (x:xs) = x : ' ' : expandir xs  

-- Ejercicio 17: Películas
-- Contamos con una base de datos de películas representada con una lista de tuplas. Cada tupla contiene la siguiente información:
-- (<Nombre de la película>, <Año de estreno>, <Duración de la película>, <Nombre del director>)
-- Observamos entonces que el tipo de la tupla que representa cada película es (String, Int, Int, String).



type Pelicula = (String, Int, Int, String)
peliculas :: [Pelicula]
peliculas = [("¿Quieres ser John Malkovich?", 1999, 112,"Spike Jonze"),
             ("¿Y donde esta el piloto?", 1980, 88,"Jim Abrahams, David Zucker"),
             ("A Clockwork Orange", 1971, 136,"Stanley Kubrick"),
             ("America X", 1998, 119,"Tony Kaye"),
             ("Amor eterno", 2004, 133,"Jean-Pierre Jeunet"),
             ("Analizame", 1999, 103,"Harold Ramis"),
             ("Asesinos por naturaleza", 1994, 118,"Oliver Stone"),
             ("Borat: El segundo mejor reportero del glorioso pais Kazajistan viaja a America", 2006, 84,"Larry Charles"),
             ("Brüno", 2009, 81,"Larry Charles"),
             ("Buenos muchachos", 1990, 146,"Martin Scorsese"),
             ("Ciudad de Dios", 2002, 130,"Fernando Meirelles, Katia Lund"),
             ("Cloud Atlas: La red invisible", 2012, 172,"Tom Tykwer, Andy Wachowski"),
             ("Delicatessen", 1991, 99,"Marc Caro, Jean-Pierre Jeunet"),
             ("Django sin cadenas", 2012, 165,"Quentin Tarantino"),
             ("El abogado del diablo", 1997, 144,"Taylor Hackford"),
             ("El ciudadano", 1941, 119,"Orson Welles"),
             ("El club de la pelea", 1999, 139,"David Fincher"),
             ("El cocodrilo", 1999, 82,"Steve Miner"),
             ("El embajador del miedo", 2004, 129,"Jonathan Demme"),
             ("El habitante incierto", 2004, 90,"Guillem Morales"),
             ("El ilusionista", 2006, 110,"Neil Burger"),
             ("El maquinista", 2004, 101,"Brad Anderson"),
             ("El mundo esta loco loco", 2001, 112,"Jerry Zucker"),
             ("El padrino", 1972, 175,"Francis Ford Coppola"),
             ("El pianista", 2002, 150,"Roman Polanski"),
             ("El plan perfecto", 2006, 129,"Spike Lee"),
             ("El resplandor", 1980, 100,"Stanley Kubric"),
             ("El senor de los anillos: La comunidad del anillo", 2001, 178,"Peter Jackson"),
             ("Estamos todos locos", 1983, 107,"Terry Jones, Terry Gilliam"),
             ("Eterno resplandor de una mente sin recuerdos", 2004, 108,"Michel Gondry"),
             ("Ganster americano", 2007, 157,"Ridley Scott"),
             ("Gran Torino", 2008, 116,"Clint Eastwood"),
             ("Guerra de los mundos", 2005, 116,"Steven Spielberg"),
             ("Hechizo del tiempo", 1993, 101,"Harold Ramis"),
             ("Historias cruzadas", 2011, 146,"Tate Taylor"),
             ("Juegos sexuales", 1999, 97,"Roger Kumble"),
             ("Kill Bill, la venganza: Volumen I", 2003, 111,"Quentin Tarantino"),
             ("La caida", 2004, 156,"Oliver Hirschbiegel"),
             ("La edad de la inocencia", 1993, 139,"Martin Scorsese"),
             ("La niebla", 2007, 126,"Frank Darabont"),
             ("La noche del demonio", 2010, 103,"James Wan"),
             ("La ola", 2008, 107,"Dennis Gansel"),
             ("La vida de Brian", 1979, 94,"Terry Jones"),
             ("La vida de los otros", 2006, 137,"Florian Henckel von Donnersmarck"),
             ("Los caballeros de la mesa cuadrada", 1975, 91,"Terry Gilliam, Terry Jones"),
             ("Los otros", 2001, 101,"Alejandro Amenabar"),
             ("Los sospechosos de siempre", 1995, 106,"Bryan Singer"),
             ("Magdalene Sisters: En el nombre de Dios", 2002, 119,"Peter Mullan"),
             ("Magnolia", 1999, 188,"Paul Thomas Anderson"),
             ("Martha Marcy May Marlene", 2011, 102,"Sean Durkin"),
             ("Matrix", 1999, 136,"The Wachowski Brothers, The Wachowski Brothers"),
             ("Mississippi en llamas", 1988, 128,"Alan Parker"),
             ("Numero 23", 2007, 101,"Joel Schumacher"),
             ("Pandillas de Nueva York", 2002, 167,"Martin Scorsese"),
             ("Perros de la calle", 1992, 99,"Quentin Tarantino"),
             ("Petroleo sangriento", 2007, 158,"Paul Thomas Anderson"),
             ("Pi", 1998, 84,"Darren Aronofsky"),
             ("Promesas del este", 2007, 100,"David Cronenberg"),
             ("Psicopata americano", 2000, 102,"Mary Harron"),
             ("Requiem para un sueno", 2000, 102,"Darren Aronofsky"),
             ("Suenos de libertad", 1994, 142,"Frank Darabont"),
             ("Taxi Driver", 1976, 113,"Martin Scorsese"),
             ("The Butcher Boy", 1997, 110,"Neil Jordan"),
             ("The Weather Man", 2005, 102,"Gore Verbinski")]


-- a) Definí la función verTodas : [(String, Int, Int, String)] → Int que dada una lista de películas devuelva el tiempo que tardaría en verlas a todas.
verTodas :: [(String, Int, Int, String)] -> Int
verTodas [] = 0
verTodas ((nombre, año, minutos, director): xs) = minutos + verTodas xs

-- b) Definí la función estrenos : [(String, Int, Int, String)] → [String] que dada una lista de películas devuelva el listado de películas que estrenaron en 2020.

estrenos :: [(String, Int, Int, String)] -> [String]
estrenos [] = []
estrenos [(nombre, 0 , minutos, director)] = []
estrenos ((nombre, año, minutos, director): xs) | año == 2020 = nombre : estrenos xs
                                                 | año /= 2020 = estrenos xs


-- c) Definí la función filmografia : [(String, Int, Int, String)] → String → [String] que dada una lista de películas y el nombre de un director, devuelva el listado de películas de ese director.

filmografia :: [(String, Int, Int, String )] -> String -> [String]
filmografia [] direName = []
filmografia ((nombre, año, minutos, director): xs) direName | director == direName = nombre : filmografia xs direName
                                                         | director /= direName = filmografia xs direName


-- d) Definí la función duracion: [(String, Int, Int, String)] → String → Int que dada una lista de películas y el nombre de una película, devuelva la duración de esa película.

duracion :: [(String, Int, Int, String)] -> String -> Int
duracion [] pelicula = 0
duracion ((nombre, año, minutos, director): xs) pelicula  | nombre == pelicula = minutos
                                                          | nombre /= pelicula = (duracion xs pelicula)
 
-- Ejercicios de finales

-- Ejercicio 18:  Definir la función recursiva dobles : [Num] → [(Num, Num)], que dada una lista de números retorna la lista resultante de armar un par con cada uno de ellos y luego ese mismo número multiplicado por dos. 
-- Ejemplo: 
-- dobles.[11, 7, 21] = [(11, 22), (7, 14), (21, 42)]


dobles :: [Int] -> [(Int, Int)]
dobles [] = []
dobles (x:xs) = (x, (x*2)) : dobles xs

-- Ejercicio 19:  Definir la función recursiva losOrozco : [Char ] → Bool que dada una lista de caracteres (una String) xs retorna True si la única vocal que aparece en xs es la ‘o’, es decir, si no aparece ninguna de las otras vocales, y False si aparece alguna otra. Ejemplos:
-- (i) losOrozco.“famaf” = False
-- (ii) losOrozco.“hoy” = True

losOrozco :: [Char] -> Bool
losOrozco [] = True
losOrozco (x:xs) = x == 'o' || (x /= 'a' && x /= 'e' && x /= 'i' && x /= 'u') && losOrozco xs

