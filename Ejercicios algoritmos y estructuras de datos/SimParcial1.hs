{-# LANGUAGE NPlusKPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant bracket" #-}

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


--Ejercicio 3: Defina el tipo EquipoFavorito como sinonimo de String, usando la definicion de listaAsoc del proyecto 2 para programar la funcion:

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