import Text.Show.Functions

data Persona = UnaPersona{
    nombre :: String,
    calorias :: Int,
    hidratacion :: Int,
    tiempo :: Int,
    equipamiento :: [String]}deriving Show

type Ejercicio = Persona -> Persona

--Maps--
mapCalorias :: (Int -> Int) -> Ejercicio
mapCalorias unaFuncion unaPersona = unaPersona{calorias = unaFuncion . calorias $ unaPersona}

perderCalorias :: Int -> Ejercicio
perderCalorias unaCantidad unaPersona = mapCalorias (subtract unaCantidad) unaPersona

mapHidratacion :: (Int -> Int) -> Ejercicio
mapHidratacion unaFuncion unaPersona = unaPersona{hidratacion = unaFuncion . hidratacion $ unaPersona}

perderHidratacion :: Int -> Ejercicio
perderHidratacion unaCantidad unaPersona = mapHidratacion (subtract unaCantidad) unaPersona

mapEquipamiento :: ([String] -> [String]) -> Ejercicio
mapEquipamiento unaFuncion unaPersona = unaPersona{equipamiento = unaFuncion . equipamiento $ unaPersona}

mapNombre :: (String -> String) -> Ejercicio
mapNombre unaFuncion unaPersona = unaPersona{nombre = unaFuncion . nombre $ unaPersona}



abdominales :: Int -> Ejercicio
abdominales unaCantidad unaPersona = mapCalorias ((-).(8*) $ unaCantidad) unaPersona

flexiones :: Int -> Ejercicio
flexiones unaCantidad unaPersona = mapCalorias ((-).(16*) $ unaCantidad) . mapHidratacion ((+). (*)2 . div 10 $ unaCantidad) $ unaPersona

levantarPesas :: Int -> Int -> Ejercicio
levantarPesas unaCantidad unPeso unaPersona 
    |tienePesa unaPersona = perderCalorias (32 * unaCantidad) . perderHidratacion ((*) unPeso . div unaCantidad $ 10) $ unaPersona
    |otherwise = unaPersona 

tienePesa :: Persona -> Bool
tienePesa unaPersona = elem "pesa" . equipamiento $ unaPersona

laGranHomeroSimpson :: Ejercicio
laGranHomeroSimpson unaPersona = unaPersona


------Parte A-----
renovarEquipo :: Persona -> Persona
renovarEquipo unaPersona = mapEquipamiento (map (reverse.(++ "oveuN").reverse)) unaPersona

volverseYoguista :: Persona -> Persona
volverseYoguista unaPersona = mapCalorias(subtract . div (calorias unaPersona) $ 2) . duplicarHidratacion . mapEquipamiento (const ["Colchoneta"]) $ unaPersona

duplicarHidratacion :: Persona -> Persona
duplicarHidratacion unaPersona
    |((*2) . hidratacion $ unaPersona) > 100 = mapHidratacion (const 100) unaPersona
    |otherwise = mapHidratacion (*2) unaPersona

volverseBodyBuilder :: Persona -> Persona
volverseBodyBuilder unaPersona
    |soloTienePesas unaPersona = mapNombre (++ "BB") . mapCalorias (*3) $ unaPersona
    |otherwise = unaPersona

soloTienePesas :: Persona -> Bool
soloTienePesas unaPersona = all (=="Pesas") . equipamiento $ unaPersona


comerUnSandwich :: Persona -> Persona
comerUnSandwich unaPersona = mapCalorias (+500) . mapHidratacion (const 100) $ unaPersona

--Parte B--
type Rutina = (Int , [Ejercicio])

rutinaSuperior :: [Ejercicio] -> Persona -> Int -> Int -> Bool
rutinaSuperior unaRutina unaPersona unValor1 unValor2 = ((caloriasPostRutina unaRutina unaPersona) < unValor1) && ((hidratacionPostRutina unaRutina unaPersona) < unValor2)

caloriasPostRutina::  [Ejercicio] -> Persona -> Int
caloriasPostRutina unaRutina unaPersona = calorias . hacerRutina unaRutina $ unaPersona 

hidratacionPostRutina :: [Ejercicio] -> Persona -> Int
hidratacionPostRutina unaRutina unaPersona = hidratacion . hacerRutina unaRutina $ unaPersona 

hacerRutina :: [Ejercicio] -> Persona -> Persona
hacerRutina unaRutina unaPersona = foldl (hacerEjercicio) unaPersona unaRutina

hacerEjercicio :: Persona -> Ejercicio -> Persona
hacerEjercicio unaPersona unEjercicio = unEjercicio unaPersona


esPeligrosa :: [Ejercicio] -> Persona -> Bool
esPeligrosa unaRutina unaPersona = rutinaSuperior unaRutina unaPersona 50 10

esBalanceada :: [Ejercicio] -> Persona -> Bool
esBalanceada unaRutina unaPersona = rutinaSuperior unaRutina unaPersona (div (calorias unaPersona) 2) 80

elAbominableAbdominal :: Rutina
elAbominableAbdominal = (60 , zipWith [1,2..] [abdominales,abdominales..])


