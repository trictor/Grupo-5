{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- para mostrar <Function> en consola
import Data.List -- para los metodos coleccionables que no vienen en la guia de lenguaje
import Data.Maybe -- por si llegan a usar un metodo de coleccion y devuelva Nothing or justElements
-- import Test.Hspec -- para usar los test


deposito dineroDepositado unaBilletera = (+) dineroDepositado unaBilletera
--extraccion  unDeposito monto = min(, 0)
--restoExtraccion deposito monto =

                    --falta terminar para el tope de 10 unidades
upgrade unaBilletera  = (+) (aumentoBilletera unaBilletera) unaBilletera
aumentoBilletera laBilletera = laBilletera * 0.2
cierreDeCuenta unaBilletera = 0
quedaIgual unaBilletera = unaBilletera

          -- usuarios
data Usuario = Usuario {
                  nombre :: String,
                  billetera :: Int
                  }deriving (Show, Eq)
pepe = Usuario "pepe" 10
lucho = Usuario "lucho" 2

        -- transacciones
-- transacciones unUsuario tipoTransaccion = unUsuario (billetera (tipoTransaccion ) )

pepe2 = Usuario "pepe2" 20
