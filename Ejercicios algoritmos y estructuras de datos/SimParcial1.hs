{-# LANGUAGE NPlusKPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Distribution.Simple (KnownExtension(NumericUnderscores))
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use camelCase" #-}

--Ejercicio 1: Definir el tipo EmpresaTelefono cuyos constructores seran Claro, Personal, Movistar y Tuenti. Defini un tipo Frase como sinonimo de String y defini la función

-- fraseEmpresa :: EmpresaTelefono -> Frase

--que a cada empresa le asocia su frase ("Claro, La red mas poderosa", "Personal, es como vos", "Movistar, Compartida la vida es mas...", "Tuenti es la mas economica"). Usar Pattern Matching

data EmpresaTelefono = Claro | Personal | Movistar | Tuenti deriving (Eq, Ord, Show)
type Frase = String 

fraseEmpresa :: EmpresaTelefono -> Frase
fraseEmpresa Claro = "Claro, La red mas poderosa"
fraseEmpresa Personal = "Personal, es como vos"
fraseEmpresa Movistar = "Movistar, Compartida la vida es mas..."
fraseEmpresa Tuenti = "Tuenti, es la mas economica"


--fraseEmpresa Movistar --> "Movistar, Compartida la vida es mas..."


--Ejercicio 2: Una persona puede tener cuenta en varias empresas, vamos a llevar el registro de las empresas de telefonos por persona. Defina el tipo NombrePersona como sinonimo de  String. Defina el tipo recursivo MisEmpresas que tiene 2 constructores:

--AgregaEmpresa: guarda que EmpresaTelefono estoy agregando, un NombrePersona y un valor de tipo MisEmpresas a la cual se le agrega la EmpresaTelefono con el NombrePersona 

--Ninguna: sin parametros y es equivalente a la lista vacia.

--luego programar la funcion 

--tengoEmpresa :: MisEmpresas -> EmpresaTelefono -> NombrePersona -> Bool

--que dado un valor del tipo MisEmpresas, una EmpresaTelefono y un NombrePersona devuelve True si la EmpresaTelefono y el NombrePersona estan en MisEmpresas, False en caso contrario.

type NombrePersona = String 
data MisEmpresas = AgregaEmpresa EmpresaTelefono NombrePersona MisEmpresas| Ninguna deriving (Show)

tengoEmpresa :: MisEmpresas -> EmpresaTelefono -> NombrePersona -> Bool
tengoEmpresa Ninguna _ _ = False
tengoEmpresa (AgregaEmpresa empresa nombre restantes) empresaBuscada nombreBuscado 
    |nombre == nombreBuscado && empresa == empresaBuscada = True
    | otherwise = tengoEmpresa restantes empresaBuscada nombreBuscado



  
  --let misEmpresas = AgregaEmpresa Claro "Adrian" (AgregaEmpresa Movistar "Adrian" Ninguna)
  --tengoEmpresa misEmpresas Claro "Adrian" --> True


  --Ejercicio 3: Defina el tipo NroTel como sinónimo de Int, usando la definicion de ListaAsoc del Proyecto 2 programar la funcion

  --agregaLA :: ListaAsoc EmpresaTelefono NroTel -> NroTel -> ListaAsoc EmpresaTelefono NroTel
  --que devuelve la lista de asociaciones a la cual le agrego la asociacion NroTel con la EmpresaTelefono

type NroTel = Int  
data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving Show

agregaLa :: ListaAsoc EmpresaTelefono NroTel -> NroTel -> ListaAsoc EmpresaTelefono NroTel
agregaLa Vacia nroTel = Nodo Claro nroTel Vacia
agregaLa (Nodo empresa telefono restantes) nroTel = Nodo empresa telefono (agregaLa restantes nroTel)


-- listas de prueba
--let listaInicial = Vacia
--let listaConNumero1 = agregaLa listaInicial 123456789
--let listaConNumero2 = agregaLa listaConNumero1 98765432

--listaConNumero2 --> Nodo Claro 123456789 (Nodo Claro 98765432 Vacia)


--Ejercicio 1' Definir el tipo Deporte cuyos constructores son Futbol, Basket, Tenis, Valorant, Dota2. Defini un tipo MinJugadores como sinonimo de Int y defini la funcion
-- minimaCantidadm:: Deporte -> MinJugadores
--Que a cada deporte le asocia la minima cantidad de jugadores necesario para jugar, por ejemplo, para jugar al tenis se necesita al menos 2 jugadores

data Deporte = Futbol | Basket | Tenis | Valorant | Dota2 deriving (Eq, Ord, Show)
type MinJugadores = Int 
minimaCandidadm:: Deporte -> MinJugadores
minimaCandidadm Futbol = 22
minimaCandidadm Basket = 10
minimaCandidadm Tenis = 2
minimaCandidadm Valorant = 4
minimaCandidadm Dota2 = 6

--minimaCandidadm Dota2 --> 6


--Ejercicio 2' Una persona puede practicar varios deportes. Defina el tipo NombrePersona' como sinonimo de String. Defina el tipo recursivo PracticoDeporte que tiene 2 constructores:

--AgregaDeporte : guarda que Deporte estoy agregand, un NombrePersona y un valor de tipo PracticoDeporte al cual se le agrega el nuevo deporte practicado junto con la persona.Applicative

--Ninguna': sin parametros y es equivalente a la lista vacia, es decir, no practica deportes

--luego programar la funcion 
--deporte :: PracticoDeporte -> Deporte -> NombrePersona -> Bool

--que dada un valor del tipo PracticoDeporte, un Deporte y un NombrePersona, devuelve True, si la persona practica el deporte, o false en caso contrario
--De un ejemplo de ejecucion que incluya su propio nombre como algun valor de tipo NombrePersona

type NombrePersona' = String 
data PracticoDeporte = AgregaDeporte Deporte NombrePersona' PracticoDeporte | Ninguna' deriving (Show)

deporte' :: PracticoDeporte -> Deporte -> NombrePersona' -> Bool
deporte' Ninguna' _ _ = False
deporte' (AgregaDeporte deporte nombre restantes') deporteBuscado nombreBuscado
        | nombre == nombreBuscado && deporte == deporteBuscado = True
        | otherwise = deporte' restantes' deporteBuscado nombreBuscado



-- let deportesPracticados = AgregaDeporte Futbol "Adrian" (AgregaDeporte Tenis "Adrian" Ninguna')
-- deporte' deportesPracticados Futbol "Adrian" --> True


--Ejercicio 3': Defina el tipo EquipoFavorito como sinonimo de String, usando la definicion de listaAsoc del proyecto 2 para programar la funcion:

--agregaEquipoFavorito :: ListaAsoc Deporte EquipoFavorito -> Deporte -> EquipoFavorito -> ListaAsoc Deporte EquipoFavorito

-- que devuelve la lista de asociaciones a la cual le agrego la asociacion EquipoFavorito de determinado Deporte

type EquipoFavorito = String


agregaEquipoFavorito :: ListaAsoc Deporte EquipoFavorito -> Deporte -> EquipoFavorito -> ListaAsoc Deporte EquipoFavorito
agregaEquipoFavorito Vacia deporte equipo = Nodo deporte equipo Vacia
agregaEquipoFavorito (Nodo d e restantes'') deporte equipo = Nodo d e (agregaEquipoFavorito restantes'' deporte equipo)


--listas auxiliares
--let listaInicial = Vacia

--let listaConFutbol = agregaEquipoFavorito listaInicial Futbol "Boca"
--let listaConTenis = agregaEquipoFavorito listaConFutbol Tenis "Nadal"
--let listaConBasket = agregaEquipoFavorito listaConTenis Basket "Lakers"

--listaConBasket --> Nodo Futbol "Boca" (Nodo Tenis "Nadal" (Nodo Basket "Lakers" Vacia))


--Parcial 2022 tema A

--Ejercicio 1''

--a)Definir el tipo Palo que consta de los constructores Treboles, Corazones, Picas,Diamantes. Los constructores no toman parámetros. El tipo Palo no debe estar en la clase Eq. Luego programa la función usando pattern matching:

--mismo_palo :: Palo -> Palo -> Bool
--que dados dos valores p1 y p2 del tipo Palo debe devolver True cuando p1 y p2 son el mismo palo (se construyen con el mismo constructor) y False en caso contrario. Si se usan más de cinco casos, este apartado sumará menos puntaje.

data Palo = Treboles | Corazones | Picas | Diamantes deriving (Show)

mismo_palo :: Palo -> Palo -> Bool 
mismo_palo Treboles Treboles = True
mismo_palo Corazones Corazones = True
mismo_palo Picas Picas = True
mismo_palo Diamantes Diamantes = True
mismo_palo _ _ = False

--b) Definir el tipo Naipe que representa una carta de poker. Tiene constructores:
-- Constructor Numerada: Toma dos parámetros, el primero de tipo Numero y el segundo de tipo Palo
-- Constructores Rey, Reina, Jota, As: Todos son constructores con un sólo parámetro de tipo Palo
--El tipo Numero debe ser un sinónimo del tipo Int.
type Numero = Int
data Naipe = Numerada Numero Palo | Rey Palo | Reina Palo | Jota Palo | As Palo  deriving (Show)


--c) Programar la función
--valor_naipe :: Naipe -> Int
--teniendo en cuenta que el valor de una carta será:
--Si es una carta numerada : Su valor es el número de la carta.
--Si es el naipe Jota : Su valor es 11
--Si es el naipe Reina : Su valor es 12
--Si es el naipe Rey : Su valor es 13
--Si es el naipe As : Su valor es 14

valor_naipe :: Naipe -> Int
valor_naipe (Jota _) = 11
valor_naipe (Reina _) = 12
valor_naipe (Rey _)= 13
valor_naipe (As _)= 14
valor_naipe (Numerada n _) = n

--valor_naipe (Jota Corazones) --> 11
--valor_naipe (Numerada 2 Corazones) --> 2

--d) Incluir el tipo Naipe en la clase Ord de manera tal que un naipe se considere mayor que otro si su valor según la función valor_naipe es más grande.
instance Eq Naipe where
    (==) :: Naipe -> Naipe -> Bool
    (Numerada n1 p1) == (Numerada n2 p2) = n1 == n2 && mismo_palo p1 p2
    (Rey p1) == (Rey p2) =  mismo_palo p1 p2
    (Reina p1) == (Reina p2) =  mismo_palo p1 p2
    (Jota p1) == (Jota p2) =  mismo_palo p1 p2
    (As p1) == (As p2)  = mismo_palo p1 p2

    _ == _ = False
instance Ord Naipe where
    compare :: Naipe -> Naipe -> Ordering
    compare naipe1 naipe2 = compare (valor_naipe naipe1) (valor_naipe naipe2) 

    --Numerada 10 Corazones > Rey Treboles --> False

    --Ejercicio 2
    
--a) Programar de manera recursiva la función

--solo_numeradas :: [Naipe] -> Palo -> [Numero]

--que dada una lista de cartas ns y un palo p devuelve una lista con los números de las cartas numeradas (las que no son ases, jotas, reyes ni reinas) de ns que son del palo p.
--b) Escribir una lista de naipes con al menos tres elementos, donde uno de ellos debe ser una figura, y otro debe ser una carta numerada.
--c) Escribir el resultado de solo_numeradas para la lista del punto b)