{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- para mostrar <Function> en consola
import Data.List -- para los metodos coleccionables que no vienen en la guia de lenguaje
import Data.Maybe -- por si llegan a usar un metodo de coleccion y devuelva Nothing or justElements
import Test.Hspec -- para usar los test


ejecutarTest = hspec $ do
      describe "Funciones basicas probadas sobre una billetera de 10" $ do
        it "Depositar 10 genera una billetera de 20" $ (billetera.deposito 10) prueba `shouldBe` 20
        it "Extraer 3 genera una billetera de 7" $ (billetera.extracción 3) prueba `shouldBe` 7
        it "Extraer 15 genera una billetera de 0" $ (billetera.extracción 15) prueba `shouldBe` 0
        it "Un upgrade genera una billetera de 12" $ (billetera.upgrade) prueba `shouldBe` 12
        it "Cerrar la cuenta genera una billetera de 0" $ (billetera.cierreDeCuenta) prueba `shouldBe` 0
        it "quedaIgual no genera modificaciones" $ (billetera.quedaIgual) prueba `shouldBe` 10
        it "Depositar 1000 y luego hacer un upgrade da 1020" $ (billetera.upgrade.deposito 1000) prueba `shouldBe` 1020
      describe "Consultas de Usuarios, sin generar nuevas funciones"
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

extracción :: Float -> Usuario -> Usuario
extracción dineroExtraido unUsuario | (billetera.restarDinero dineroExtraido) unUsuario  <=0  = unUsuario{billetera = 0}
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

--Usuario de prueba a fin de no volver a repetir a pepe ya que es pedido como prueba especifica
--en el caso de uso 8 en adelante

prueba = Usuario "prueba" 10

--Transaccciones 1 y 2--

transacción1 unUsuario | nombre unUsuario == "Lucho" = cierreDeCuenta unUsuario
                       | otherwise = quedaIgual unUsuario

transacción2 unUsuario | nombre unUsuario == "Pepe"  = deposito 5 unUsuario
                       | otherwise = quedaIgual unUsuario

-- nuevos eventos
-- de hecho estoy dudando si va esto, basicamente lo puse por que dice agregar como funciones el tocoYmeVoy y ahorranteErrante
-- ya que sino, esto basica-mente se puede probar en consola con composicion y es idem!
tocoYmeVoy unUsuario = (cierreDeCuenta.upgrade.deposito 15) unUsuario


ahorranteErrante unUsuario = (deposito 10.upgrade.deposito 8. extracción 1. deposito 2.deposito 1) unUsuario

--transacciones de prueba pedidas a modo de prueba por enunciado
transacción3 unUsuario = tocoYmeVoy unUsuario
transacción4 unUsuario = ahorranteErrante unUsuario

-- transaccción mas compleja

transacción5 unUsuario | nombre unUsuario == "Pepe" = extracción 7 unUsuario
                       | nombre unUsuario == "Lucho" = deposito 7 unUsuario
                       | otherwise = quedaIgual unUsuario
