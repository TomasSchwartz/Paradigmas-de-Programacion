import Text.Show.Functions

data Jugador = UnJugador{
    nombre :: String , 
    cantidadDinero :: Int , 
    estrategiaDeJuego :: String , 
    propiedadesDeLasPropiedades :: [Propiedades], 
    acciones :: [Accion]
}deriving (Show)
type Propiedades = (String , Int)
type Accion = Jugador -> Jugador

carolina :: Jugador
carolina = (UnJugador "Carolina" 500 "Accionista" [] [pagarAAccionistas])

--maps--
funcion :: Jugador -> Jugador
funcion unJugador = unJugador

mapNombre:: (String -> String) -> Jugador -> Jugador
mapNombre unaFuncion unJugador = unJugador{nombre = unaFuncion.nombre $ unJugador}

mapCantidadDinero:: (Int -> Int) -> Jugador -> Jugador
mapCantidadDinero unaFuncion unJugador = unJugador{cantidadDinero = unaFuncion.cantidadDinero $ unJugador}

mapEstrategiaDeJuego:: (String -> String) -> Jugador -> Jugador
mapEstrategiaDeJuego unaFuncion unJugador = unJugador{estrategiaDeJuego = unaFuncion.estrategiaDeJuego $ unJugador}

mapPropieadesDeCompra:: ([Propiedades] -> [Propiedades]) -> Jugador -> Jugador
mapPropieadesDeCompra unaFuncion unJugador = unJugador{propiedadesDeLasPropiedades = unaFuncion.propiedadesDeLasPropiedades $ unJugador}

mapAcciones:: ([Accion] -> [Accion]) -> Jugador -> Jugador
mapAcciones unaFuncion unJugador = unJugador{acciones = unaFuncion.acciones $ unJugador}
--maps--


pasarPorElBanco :: Accion
pasarPorElBanco unJugador = aumentarDinero 40 . cambiarTactica "Comprador compulsivo" $ unJugador

enojarse :: Accion
enojarse unJugador = aumentarDinero 50 . agregarAccion gritar $ unJugador

gritar :: Accion
gritar unJugador = agregarAlPrincipioDelNombre "AHHHH" unJugador

subasta :: Int -> Propiedades -> Accion 
subasta unPrecio unaPropiedad (UnJugador nombre cantidadDinero "Oferente singular" propiedadesDeLasPropiedades acciones ) = (UnJugador nombre (cantidadDinero - unPrecio) "Oferente singular" ((:) unaPropiedad propiedadesDeLasPropiedades) acciones)
subasta unPrecio unaPropiedad (UnJugador nombre cantidadDinero "Accionista" propiedadesDeLasPropiedades acciones ) = (UnJugador nombre (cantidadDinero - unPrecio)  "Accionista" ((:) unaPropiedad propiedadesDeLasPropiedades) acciones)
subasta _ _ unJugador = unJugador


cobrarAlquileres :: Jugador -> Int
cobrarAlquileres unJugador = sum (precioDePropiedades unJugador)

pagarAAccionistas :: Accion
pagarAAccionistas (UnJugador nombre cantidadDinero "Accionista" propiedadesDeLasPropiedades acciones) = (UnJugador nombre (cantidadDinero + 200) "Accionista" propiedadesDeLasPropiedades acciones)
pagarAAccionistas unJugador = aumentarDinero (-200) unJugador

hacerBerrinchePor :: Propiedades -> Accion 
hacerBerrinchePor unaPropiedad unJugador = (conseguirDineroDespuesDePropiedad unJugador unaPropiedad) . hacerMuchasAcciones  (take (dineroNecesarioParaUnaPropiedad unaPropiedad unJugador ) (repeat gritar)) . mapPropieadesDeCompra ((:) unaPropiedad) $ unJugador

ultimaRonda :: Accion
ultimaRonda unJugador = (devolverAcciones (acciones unJugador)) unJugador

devolverAcciones ::  [Accion]  -> Accion
devolverAcciones unaListaDeAcciones  = foldl (.) (accionNula) unaListaDeAcciones

juegoFinal :: Jugador -> Jugador -> Jugador
juegoFinal unJugador otroJugador
    | cantidadDinero (ultimaRonda unJugador) > cantidadDinero (ultimaRonda otroJugador) = unJugador
    | otherwise = otroJugador


accionNula :: Accion
accionNula unJugador = unJugador

--(a -> b -> a) -> a -> [b] -> a
--(Accion -> b -> Accion) -> Accion -> [Accion] -> Accion
--(foldl (aplicarAccion unJugador) [] (take unaCantidad (repeat gritar)))


conseguirDineroDespuesDePropiedad :: Jugador -> Propiedades -> Accion
conseguirDineroDespuesDePropiedad unJugador unaPropiedad
    | (cantidadDinero unJugador<).(snd) $ unaPropiedad = mapCantidadDinero (const 0)
    | otherwise = mapCantidadDinero . const . ((-) (cantidadDinero unJugador)) . snd $ unaPropiedad

dineroNecesarioParaUnaPropiedad :: Propiedades -> Jugador -> Int
dineroNecesarioParaUnaPropiedad unaPropiedad unJugador = div ((snd unaPropiedad) - (cantidadDinero unJugador)) 10

hacerMuchasAcciones ::  [Accion] -> Jugador -> Jugador
hacerMuchasAcciones unaListaDeAcciones unJugador = foldl (aplicarAccion) unJugador unaListaDeAcciones

--(a -> b -> a) -> a -> [b] -> a
--(Jugador -> Accion -> Jugador) -> Jugador -> [Accion] -> Jugador
--(foldl (aplicarAccion unJugador) [] (take unaCantidad (repeat gritar)))



aplicarAccion :: Jugador -> Accion -> Jugador
aplicarAccion unJugador unaAccion = unaAccion unJugador

aumentarDinero :: Int -> Accion
aumentarDinero unNumero unJugador = mapCantidadDinero (+unNumero) unJugador

cambiarTactica :: String -> Accion
cambiarTactica unaTactica unJugador = mapEstrategiaDeJuego (const unaTactica) unJugador

agregarAccion :: Accion -> Accion
agregarAccion unaAccion unJugador = mapAcciones ((:) unaAccion) unJugador

agregarAlPrincipioDelNombre :: String -> Accion
agregarAlPrincipioDelNombre unaPalabra unJugador = mapNombre (unaPalabra ++) unJugador

precioDePropiedades :: Jugador -> [Int]
precioDePropiedades unJugador = map esPrecioBaratoOCaro (propiedadesDeLasPropiedades unJugador)

esPrecioBaratoOCaro :: Propiedades -> Int
esPrecioBaratoOCaro unaPropiedad
    |snd unaPropiedad < 150 = 10
    |otherwise = 20