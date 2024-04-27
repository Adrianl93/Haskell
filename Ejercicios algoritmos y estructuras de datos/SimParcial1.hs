{-# LANGUAGE NPlusKPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import System.Win32 (COORD(xPos))
import Data.Data (Data)
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


--Ejercicio 2: Una persona puede tener cuenta en varias empresas, vamos a llevar el registro de las empresas de telefinos por persona. Defina el tipo NombrePersona como sinonimo de  String. Defina el tipo recursivo MisEmpresas que tiene 2 constructores:

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

  --agregaLA :: ListaAsoc EmpresaTelefino NroTel -> NroTel -> ListaAsoc EmpresaTelefono NroTel
  --que devuelve la lista de asociaciones a la cual le agrego la asociacion NroTel con la EmpresaTelefono
