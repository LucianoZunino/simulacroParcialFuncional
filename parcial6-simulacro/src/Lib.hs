--13:15
import Data.Char (isUpper)
--------------
-- PUNTO 01 --
--------------

data Plomero = Plomero {
    nombre :: String,
    cajaDeHerramientas :: [Herramienta],
    historial :: [String],
    dinero :: Int
}

data Herramienta = Herramienta {
    denominacion :: String,
    precio :: Int,
    empuñadura :: Material
} deriving (Show, Eq)

data Material = Hierro | Madera | Plastico | Goma deriving (Show, Eq)

mario :: Plomero
mario = Plomero {
    nombre = "Mario",
    cajaDeHerramientas =[llaveInglesaConMangoDeHierro, martilloConEmpuñaduraDeMadera],
    historial = [],
    dinero = 1200
}

llaveInglesaConMangoDeHierro :: Herramienta
llaveInglesaConMangoDeHierro = Herramienta{
    denominacion = "llave Inglesa con mango de hierro",
    precio = 200,
    empuñadura = Hierro
}

martilloConEmpuñaduraDeMadera :: Herramienta
martilloConEmpuñaduraDeMadera = Herramienta {
    denominacion = "martillo con empuñadura de madera",
    precio = 20,
    empuñadura = Madera
}

wario :: Plomero
wario = Plomero{
    nombre = "Wario",
    cajaDeHerramientas = infinitasLlaves llaveFrancesaConMangoDeHierro [],
    historial = [],
    dinero = 1
}

infinitasLlaves :: Herramienta -> [Herramienta] -> [Herramienta]
infinitasLlaves llave [] = [llave]
infinitasLlaves llave llaves = ( llave : infinitasLlaves (inflacion 1 llave) llaves)

llaveFrancesaConMangoDeHierro :: Herramienta
llaveFrancesaConMangoDeHierro = Herramienta {
    denominacion = "llave Francesa Con Mango De Hierro",
    precio = 1,
    empuñadura = Hierro
}

inflacion :: Int -> Herramienta -> Herramienta
inflacion valorAgregado llave = llave { precio = valorAgregado + (precio llave)} 


--------------
-- PUNTO 02 --
--------------

tiene :: String -> Plomero -> Bool
-- tiene denominacionHerramienta plomero = any  (== denominacionHerramienta) . denominacion . cajaDeHerramientas  plomero
tiene denominacionBuscada plomero =  any (== denominacionBuscada) . map denominacion $ (cajaDeHerramientas plomero)

esMalvado :: Plomero -> Bool
esMalvado plomero = tieneSufijoWa plomero

tieneSufijoWa :: Plomero -> Bool
tieneSufijoWa plomero = take 2 (nombre plomero) == "Wa"

puedeComprar :: Herramienta -> Plomero -> Bool
puedeComprar herramienta plomero = precio herramienta < dinero plomero

--------------
-- PUNTO 03 --
--------------

esBuenaHerramienta :: Herramienta -> Bool
esBuenaHerramienta herramienta = esDeHierroYCara herramienta || esMartilloDeMaderaOGoma herramienta

esDeHierroYCara :: Herramienta -> Bool
esDeHierroYCara herramienta = ((precio herramienta) > 10000) && (empuñadura herramienta == Hierro)

esMartilloDeMaderaOGoma :: Herramienta -> Bool
esMartilloDeMaderaOGoma herramienta = denominacion herramienta == denominacion martilloConEmpuñaduraDeMadera || denominacion herramienta == denominacion martilloConEmpuñaduraDeGoma


martilloConEmpuñaduraDeGoma :: Herramienta 
martilloConEmpuñaduraDeGoma = Herramienta {
    denominacion = "martillo Con Empuñadura De Goma",
    precio = 0,
    empuñadura = Goma
}

--------------
-- PUNTO 04 --
--------------

comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta herramienta plomero
    | puedeComprar herramienta plomero = pagar (precio herramienta) . agregarHerramienta herramienta $ plomero
    | otherwise                        = plomero

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta herramienta plomero = plomero {cajaDeHerramientas = (herramienta : (cajaDeHerramientas plomero))}

pagar :: Int -> Plomero -> Plomero
pagar precioApagar plomero = plomero {dinero = (dinero plomero)- precioApagar}
--------------
-- PUNTO 05 --
--------------

data Reparacion = Reparacion {
    descripcion :: String,
    requerimiento :: Requerimiento
}

type Requerimiento = Plomero -> Bool

filtracionDeAgua :: Reparacion
filtracionDeAgua = Reparacion {
    descripcion = "filtracion de agua",
    requerimiento = tenerLlaveInglesa
}

tenerLlaveInglesa :: Plomero -> Bool
tenerLlaveInglesa plomero = tiene "Llave inglesa" plomero

reparacionDificil :: Reparacion -> Bool
reparacionDificil reparacion = ( 100 < length (descripcion reparacion)) || all isUpper (descripcion reparacion)

presupuesto :: Reparacion -> Int
presupuesto reparacion =  (* 3 ). length $ (descripcion reparacion)

--------------
-- PUNTO 06 --
--------------

hacer :: Plomero -> Reparacion -> Plomero
hacer plomero  reparacion 
    | (((requerimiento reparacion) plomero)) = cobrar (presupuesto reparacion ). efectoDeLaReparacion reparacion $ plomero
    | esMalvadoConMartillo plomero = cobrar (presupuesto reparacion) . efectoMalvado $ plomero
    | otherwise                          = cobrar 100 plomero

cobrar :: Int -> Plomero -> Plomero
cobrar precio plomero = plomero { dinero = precio + (dinero plomero)}

efectoMalvado :: Plomero -> Plomero
efectoMalvado plomero = robarHerramienta destornilladorConMangoDePlastico plomero

efectoDeLaReparacion :: Reparacion -> Plomero ->  Plomero
efectoDeLaReparacion reparacion plomero 
    | (reparacionDificil reparacion) = pierdeBuenasHerramientas plomero 
    | otherwise                      =  plomero {cajaDeHerramientas = tail (cajaDeHerramientas plomero)}



robarHerramienta ::  Herramienta -> Plomero -> Plomero
robarHerramienta herramienta plomero = agregarHerramienta herramienta plomero

destornilladorConMangoDePlastico :: Herramienta
destornilladorConMangoDePlastico = Herramienta {
    denominacion = "destornillador Con Mango De Plastico",
    precio = 0,
    empuñadura = Plastico
}
pierdeBuenasHerramientas :: Plomero -> Plomero
pierdeBuenasHerramientas plomero = plomero {cajaDeHerramientas = filter (not . esBuenaHerramienta) . cajaDeHerramientas $ plomero}

esMalvadoConMartillo :: Plomero -> Bool
esMalvadoConMartillo plomero = esMalvado plomero && tiene "martillo" plomero

--------------
-- PUNTO 07 --
--------------

type Jornada = [Reparacion]

jornadaDeTrabajo :: Jornada -> Plomero -> Plomero
jornadaDeTrabajo jornada plomero = foldl hacer plomero jornada


--------------
-- PUNTO 08 --
--------------
empleadoMasReparador :: Jornada -> [Plomero]-> Plomero
empleadoMasReparador  jornada plomeros = empleadoBeneficiado (head plomeros) condicionDelMasReparador . map (jornadaDeTrabajo jornada) $ plomeros

empleadoBeneficiado :: Plomero -> (Plomero -> Plomero -> Bool) -> [Plomero] -> Plomero
empleadoBeneficiado plomeroAnterior _ [] = plomeroAnterior
empleadoBeneficiado plomeroAnterior funcion (plomeroActual : resto)
    | funcion plomeroAnterior plomeroActual = empleadoBeneficiado plomeroAnterior funcion resto
    | otherwise                             = empleadoBeneficiado plomeroActual funcion resto

condicionDelMasReparador :: Plomero -> Plomero -> Bool
condicionDelMasReparador plomero1 plomero2 = (length . historial $ plomero1) > (length . historial $ plomero2)

empleadoMasAdinerado :: Jornada -> [Plomero]-> Plomero
empleadoMasAdinerado jornada plomeros = empleadoBeneficiado (head plomeros) condicionDelMasAdinerado . map (jornadaDeTrabajo jornada) $ plomeros

condicionDelMasAdinerado :: Plomero -> Plomero -> Bool
condicionDelMasAdinerado plomero1 plomero2 = ( dinero  plomero1) > ( dinero $ plomero2)

empleadoMasInversionista :: Jornada -> [Plomero]-> Plomero
empleadoMasInversionista jornada plomeros = empleadoBeneficiado (head plomeros) condicionDelMasInversionista . map (jornadaDeTrabajo jornada) $ plomeros

condicionDelMasInversionista :: Plomero -> Plomero -> Bool
condicionDelMasInversionista plomero1 plomero2 = invertidoEnHerramienta plomero1 > invertidoEnHerramienta plomero2
invertidoEnHerramienta :: Plomero -> Int
invertidoEnHerramienta  = sum . map precio . cajaDeHerramientas 

-- 16:11 --