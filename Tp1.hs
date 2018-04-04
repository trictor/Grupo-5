{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- para mostrar <Function> en consola
import Data.List -- para los metodos coleccionables que no vienen en la guia de lenguaje
import Data.Maybe -- por si llegan a usar un metodo de coleccion y devuelva Nothing or justElements
-- import Test.Hspec -- para usar los test


mostrarBilletera usuario = usuario

sumarDinero :: Float -> Usuario -> Usuario
sumarDinero unDinero unUsuario = unUsuario {
            billetera = billetera unUsuario + unDinero
}

restarDinero :: Float -> Usuario -> Usuario
restarDinero unDinero unUsuario = unUsuario{
             billetera = billetera unUsuario - unDinero
}

deposito :: Float -> Usuario -> Usuario
deposito dineroDepositado unUsuario = sumarDinero dineroDepositado unUsuario

extraccion :: Float -> Usuario -> Usuario
extraccion dineroExtraido unUsuario | (billetera.restarDinero dineroExtraido) unUsuario <=0  = unUsuario{billetera = 0}
                                    | otherwise = (restarDinero dineroExtraido unUsuario)


                    --falta terminar para el tope de 10 unidades
--upgrade unaBilletera  = (+) (aumentoBilletera unaBilletera) unaBilletera
--aumentoBilletera laBilletera = laBilletera * 0.2
--cierreDeCuenta unaBilletera = 0
--quedaIgual unaBilletera = unaBilletera

          -- usuarios
data Usuario = Usuario {
                  nombre :: String,
                  billetera :: Float
                  }deriving (Show, Eq)

pepe = Usuario "pepe" 10
lucho = Usuario "lucho" 2

        -- transacciones
-- transacciones unUsuario tipoTransaccion = unUsuario (billetera (tipoTransaccion ) )

pepe2 = Usuario "pepe2" 20
