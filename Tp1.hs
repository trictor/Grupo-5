{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- para mostrar <Function> en consola
import Data.List -- para los metodos coleccionables que no vienen en la guia de lenguaje
import Data.Maybe -- por si llegan a usar un metodo de coleccion y devuelva Nothing or justElements
-- import Test.Hspec -- para usar los test


--Basicamente termine las funciones de cada evento

sumarDinero :: Float -> Usuario -> Usuario
sumarDinero unDinero unUsuario = unUsuario {
            billetera = billetera unUsuario + unDinero
}

restarDinero :: Float -> Usuario -> Usuario
restarDinero unDinero unUsuario = unUsuario{
             billetera = billetera unUsuario - unDinero
}

hacerAumento unUsuario = unUsuario {
             billetera = billetera unUsuario + billetera unUsuario * 1.20 }

esMayorADiez unUsuario = (billetera.hacerAumento) unUsuario >= (billetera.sumarDinero 10) unUsuario

deposito :: Float -> Usuario -> Usuario
deposito dineroDepositado = sumarDinero dineroDepositado

extraccion :: Float -> Usuario -> Usuario
extraccion dineroExtraido unUsuario | (billetera.restarDinero dineroExtraido) unUsuario  <=0  = unUsuario{billetera = 0}
                                    | otherwise = restarDinero dineroExtraido unUsuario

upgrade unUsuario | esMayorADiez unUsuario = sumarDinero 10 unUsuario
                  | otherwise = hacerAumento unUsuario

cierreDeCuenta unUsuario = unUsuario {
               billetera = 0
}

quedaIgual unUsuario = unUsuario

data Usuario = Usuario {
        nombre :: String,
        billetera :: Float
        }deriving (Show, Eq)

pepe = Usuario "pepe" 10
lucho = Usuario "lucho" 2

-- transacciones
-- transacciones unUsuario tipoTransaccion = unUsuario (billetera (tipoTransaccion ) )

pepe2 = Usuario "pepe2" 20
