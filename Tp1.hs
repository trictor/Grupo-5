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
             billetera = billetera unUsuario * 1.20}

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

--Usuarios

pepe = Usuario "Pepe" 10
lucho = Usuario "Lucho" 2
pepe2 = Usuario "Pepe" 20

-- nuevos eventos
-- de hecho estoy dudando si va esto, basicamente lo puse por que dice agregar como funciones el tocoYmeVoy y ahorranteErrante
-- ya que sino, esto basica-mente se puede probar en consola con composicion y es idem!
tocoYmeVoy unUsuario = (cierreDeCuenta.upgrade.deposito 15) unUsuario


ahorranteErrante unUsuario = (deposito 10.upgrade.deposito 8. extraccion 1. deposito 2.deposito 1) unUsuario


transaccion1 unUsuario | nombre unUsuario == "Lucho" = cierreDeCuenta unUsuario
                       | otherwise = quedaIgual unUsuario

transaccion2 unUsuario | nombre unUsuario == "Pepe"  = deposito 5 unUsuario
                       | otherwise = quedaIgual unUsuario
