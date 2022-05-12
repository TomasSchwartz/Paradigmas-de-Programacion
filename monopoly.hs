data Jugador = UnJugador{nombre :: String , cantidadDinero :: Int , estrategiaDeJuego :: String , propiedadesDeLasPropiedades :: [Propiedades] , acciones :: [Accion]}
type Propiedades = (String , Int)
type Accion = Jugador -> Jugador

--maps--
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

--pagarAAccionistas :: Accion

hacerBerrinchePor :: Propiedades -> Accion
hacerBerrinchePor unaPropiedad unJugador = aumentarDinero 10 . gritar $ unJugador



aumentarDinero :: Int -> Accion
aumentarDinero unNumero unJugador = mapCantidadDinero (+unNumero) unJugador

cambiarTactica :: String -> Accion
cambiarTactica unaTactica unJugador = mapEstrategiaDeJuego (const unaTactica) unJugador

agregarAccion :: Accion -> Accion
agregarAccion unaAccion unJugador = mapAcciones ((:) unaAccion) unJugador

agregarAlPrincipioDelNombre :: String -> Accion
agregarAlPrincipioDelNombre unaPalabra unJugador = mapNombre (++ unaPalabra) unJugador

precioDePropiedades :: Jugador -> [Int]
precioDePropiedades unJugador = map esPrecioBaratoOCaro (propiedadesDeLasPropiedades unJugador)

esPrecioBaratoOCaro :: Propiedades -> Int
esPrecioBaratoOCaro unaPropiedad
    |snd unaPropiedad < 150 = 10
    |otherwise = 20