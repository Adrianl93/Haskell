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


--Ejercicio 5 Definición de clases

--a) Implementa la funcion sonidoNatural como esta definida arriba.

sonidoNatural :: NotaBasica -> Int
sonidoNatural Do = 0
sonidoNatural Re = 2
sonidoNatural Mi = 4
sonidoNatural Fa = 5
sonidoNatural Sol = 7
sonidoNatural La = 9
sonidoNatural Si = 11


--b) Definir el tipo enumerado Alteracion que consta de los constructores Bemol, Natural y Sostenido.

data Alteracion = Bemol | Natural  | Sostenido deriving (Eq, Ord, Show)


--c) Definir el tipo NotaMusical que consta de un unico constructor que toma dos parametros. El primer parametro es de tipo NotaBasica y el segundo de tipo Alteracion. De esta manera cuando se quiera representar una nota alterada se puede usar como segundo parametro del constructor un Bemol o Sostenido y si se quiere representaruna nota sin alteraciones se usa Natural como segundo parametro.

data NotaMusical =  NotaMusical NotaBasica Alteracion deriving (Eq,Ord,Show)


--d)Definı la funcion sonidoCromatico :: NotaMusical -> Int que devuelve el sonido de una nota, incrementando en uno su valor si tiene la alteracion Sostenido, decrementando en uno si tiene la alteracion Bemol y dejando su valor intacto si la alteraciones Natural

sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (NotaMusical nota Natural) = sonidoNatural nota
sonidoCromatico (NotaMusical nota Bemol) = sonidoNatural nota - 1
sonidoCromatico (NotaMusical nota Sostenido) = sonidoNatural nota +1

-- declaro estos valores en GHCI

--    let nota1 = NotaMusical Do Natural
--        nota2 = NotaMusical Re Bemol
--        nota3 = NotaMusical Sol Sostenido
--        nota4 = NotaMusical Re Bemol


   --sonidoCromatico nota 1 --> 0
   --sonidoCromatico nota 2 --> 1
   --sonidoCromatico nota 3 --> 8

--e) Incluı el tipo NotaMusical a la clase Eq de manera tal que dos notas que tengan el mismo valor de sonidoCromatico se consideren iguales.

--sonidoCromatico nota2 == sonidoCromatico nota4  --> True

--f) Incluı el tipo NotaMusical a la clase Ord definiendo el operador <=. Se debe definir que una nota es menor o igual a otra si y solo si el valor de sonidoCromatico para la primera es menor o igual al valor de sonidoCromatico para la segunda.

--sonidoCromatico nota1 > sonidoCromatico nota2
--sonidoCromatico nota1 <= sonidoCromatico nota2 



--Ejercicio 6 Tipos enumerados con polimorfismo

--a) Definir la funcion primerElemento que devuelve el primer elemento de una lista no vacıa, o Nothing si la lista es vacıa.

primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x : _) = Just x

--Ejercicio 7 Tipos Recursivos

data Cola = VaciaC | Encolada Deportista Cola 

--a) Programa las siguientes funciones:

-- -- Definición de algunos deportistas para testeo


deportista1 :: Deportista
deportista1 = Velocista 170

deportista2 :: Deportista
deportista2 = Tenista DosManos Izquierda 180

deportista3 :: Deportista
deportista3 = Futbolista Delantera 10 Derecha 175


-- Cola vacía
colaVacia :: Cola
colaVacia = VaciaC

-- Cola con algunos deportistas
miCola :: Cola
miCola = encolar deportista1 (encolar deportista2 (encolar deportista3 colaVacia))

-- 1) atender :: Cola -> Maybe Cola, que elimina de la cola a la persona que esta en la primer posicion de una cola, por haber sido atendida. Si la cola esta vacıa,devuelve Nothing.

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada _ cola) = Just cola

--2) encolar :: Deportista -> Cola -> Cola, que agrega una persona a una cola de deportistas, en la ultima posicion.

encolar :: Deportista -> Cola -> Cola
encolar deportista VaciaC = Encolada deportista VaciaC
encolar deportista (Encolada dep colaRestante) = Encolada dep (encolar deportista colaRestante)


--3) busca :: Cola -> Zona -> Maybe Deportista, que devuelve el/la primera futbolista dentro de la cola que juega en la zona que se corresponde con el segundopaametro. Si no hay futbolistas jugando en esa zona devuelve Nothing.

busca :: Cola -> Zona -> Maybe Deportista
busca VaciaC _ = Nothing
busca (Encolada deportista colaRestante) zona =
    case deportista of
        Futbolista zonaDeporte _ _ _ | zonaDeporte == zona -> Just deportista
        _ -> busca colaRestante zona

--b) ¿A que otro tipo se parece Cola?

-- Cola podria parecerse bastante a una Lista, con la diferencia de que no hay acceso rapido al último elemento de la cola, y su funcionamiento es del tipo First In First Out similar a una cola de supermercado, o en inglés Queue.




--Ejercicio 8 Tipos recursivos y polimórficos


data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving Show
type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

-- Listas aux para testeo

listaEjemplo :: ListaAsoc String Int
listaEjemplo = Nodo "clave1" 1 (Nodo "clave2" 2 Vacia)

listaEjemplo2 :: ListaAsoc Char Int
listaEjemplo2 = Nodo 'a' 1 (Nodo 'b' 2 (Nodo 'c' 3 Vacia))



listaVacia :: ListaAsoc Int String
listaVacia = Vacia

listaUno :: ListaAsoc Int String
listaUno = Nodo 1 "dato1" Vacia

listaTres :: ListaAsoc Int String
listaTres = Nodo 1 "dato1" (Nodo 2 "dato2" (Nodo 3 "dato3" Vacia))

-- a) ¿Como se debe instanciar el tipo ListaAsoc para representar la informacion almacenada en una guıa telefonica?

type GuiaTelefonica = ListaAsoc String String

--b) Programa las siguientes funciones:

--1) la_long :: ListaAsoc a b -> Int que devuelve la cantidad de datos en una lista.

la_long :: ListaAsoc a b -> Int 
la_long Vacia = 0
la_long (Nodo _ _ resto) = 1 + la_long resto


--la_long listaVacia --> 0
--la_long listaUno --> 1
--la_long listaTres --> 3

--2)la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b, que devuelve la concatenacion de dos listas de asociaciones.

la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia lista = lista
la_concat (Nodo clave valor resto1) lista2 = Nodo clave valor (la_concat resto1 lista2)

--la_concat listaUno listaTres   --> Nodo 1 "dato1" (Nodo 1 "dato1" (Nodo 2 "dato2" (Nodo 3 "dato3" Vacia)))
--la_concat listaVacia listaTres --> Nodo 1 "dato1" (Nodo 2 "dato2" (Nodo 3 "dato3" Vacia))

--3)la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b, que agrega un nodo a la lista de asociaciones si la clave no esta en la lista, o actualiza el valor si la clave ya se encontraba.


la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar Vacia nuevaClave nuevoValor = Nodo nuevaClave nuevoValor Vacia
la_agregar (Nodo clave valor resto) nuevaClave nuevoValor
    | clave == nuevaClave = Nodo clave nuevoValor resto
    | otherwise = Nodo clave valor (la_agregar resto nuevaClave nuevoValor)


--la_agregar listaEjemplo "clave3" 3  --> Nodo "clave1" 1 (Nodo "clave2" 2 (Nodo "clave3" 3 Vacia))
--la_agregar listaEjemplo "clave1" 10 --> Nodo "clave1" 10 (Nodo "clave2" 2 Vacia)


--4) la_pares :: ListaAsoc a b -> [(a, b)] que transforma una lista de asociaciones en una lista de pares clave-dato.

la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo clave valor resto) = (clave, valor) : la_pares resto


pares :: [(Char, Int)]
pares = la_pares listaEjemplo2

--print pares --> [('a',1),('b',2),('c',3)]


-- 5)  la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b que dada una lista y una clave devuelve el dato asociado, si es que existe. En caso contrario devuelve Nothing.

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia _ = Nothing
la_busca (Nodo clave valor resto) buscada
    | clave == buscada = Just valor
    | otherwise = la_busca resto buscada

    --la_busca listaEjemplo "clave2" --> Just 2
    --la_busca listaEjemplo "clave4" --> Nothing



-- 6) la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b que dada una clave a elimina la entrada en la lista

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar _ Vacia = Vacia
la_borrar a (Nodo clave valor resto)
    | clave == a = resto
    | otherwise = Nodo clave valor (la_borrar a resto)

    --listaBorrada = la_borrar "clave2" listaEjemplo
    -- print listaBorrada --> Nodo "clave1" 1 Vacia
