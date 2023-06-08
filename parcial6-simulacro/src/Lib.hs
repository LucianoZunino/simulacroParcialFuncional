--13: 13

--------------
-- PUNTO 01 --
--------------

data Plomero = Plomero {
    nombre :: String,
    cajaDeHerramientas :: [Herramienta],
    historial :: [String],
    dinero :: Float
}

data Herramienta = Herramienta {
    denominacion :: String,
    precio :: Float,
    empuñadura :: Material
} deriving (Show)

data Material = Hierro | Madera | Plastico | Goma deriving (Show)

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
    dinero = 0.50
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

inflacion :: Float -> Herramienta -> Herramienta
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