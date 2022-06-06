------------------------------------------------Parte A------------------------------------------------
import Text.Show.Functions

data Cancion = UnaCancion{titulo :: String, genero :: Genero , duracion :: Int} deriving (Show)
type Genero = String

data Artista = UnArtista{nombre::String, canciones :: [Cancion], efectoPreferido :: Efecto} deriving (Show)
type Efecto = Cancion -> Cancion
--Maps
mapDuracion :: (Int -> Int) -> Cancion ->  Cancion
mapDuracion  unaFuncion unaCancion = unaCancion{duracion = unaFuncion.duracion $ unaCancion}

mapNombre ::  (String -> String) -> Cancion  -> Cancion
mapNombre unaFuncion unaCancion  = unaCancion{titulo = unaFuncion.titulo $ unaCancion}

mapGenero :: (Genero -> Genero) -> Cancion -> Cancion
mapGenero unaFuncion unaCancion = unaCancion{genero = unaFuncion.genero $ unaCancion}

--funciones con map
cambiarGenero :: Genero -> Cancion -> Cancion
cambiarGenero unGenero unaCancion = mapGenero (const unGenero) unaCancion

cambiarDuracion :: Int -> Cancion -> Cancion
cambiarDuracion unaDuracion unaCancion = mapDuracion (const unaDuracion) unaCancion

--Efectos
acortar :: Efecto
acortar unaCancion 
    |duracion unaCancion < 60 = unaCancion
    |otherwise = mapDuracion  (subtract 60) unaCancion

remixear :: Efecto
remixear unaCancion = mapNombre (++ " remix") . mapDuracion (*2) . cambiarGenero "Remixeado" $ unaCancion

acustizar :: Int -> Efecto
acustizar _ (UnaCancion titulo "acústico" duracion) = (UnaCancion titulo "acústico" duracion)
acustizar unaDuracion unaCancion = cambiarGenero "acústico" . cambiarDuracion unaDuracion $ unaCancion

metaEfecto :: [Efecto] -> Cancion -> Cancion
metaEfecto unaListaDeEfectos unaCancion = foldl (aplicarEfecto) unaCancion unaListaDeEfectos

aplicarEfecto :: Cancion -> Efecto -> Cancion
aplicarEfecto unaCancion unEfecto = unEfecto unaCancion

--Modelar canciones y artistas
cafeParaDos :: Cancion
cafeParaDos = UnaCancion{titulo = "Café para dos" , genero = "Rock melancólico" , duracion = 146}

fuiHastaAhi :: Cancion
fuiHastaAhi = UnaCancion{titulo = "Fuí hasta ahí" , genero = "Rock" , duracion = 279}

losEscarabajos :: Artista
losEscarabajos = UnArtista{nombre = "Los Escarabajos" , canciones = [rocketRaccoon, mientrasMiBateriaFesteje, tomateDeMadera] , efectoPreferido = acortar}

rocketRaccoon :: Cancion
rocketRaccoon = UnaCancion{titulo = "Rocket Raccoon", genero = "" , duracion = 100}
mientrasMiBateriaFesteje :: Cancion
mientrasMiBateriaFesteje = UnaCancion{titulo = "Mientras mi bateria festeje" , genero = "" , duracion = 100}
tomateDeMadera :: Cancion
tomateDeMadera = UnaCancion{titulo = "Tomate de Madera" , genero = "" , duracion = 100}

adela :: Artista
adela = UnArtista{nombre = "Adela" , canciones = [teAcordas, unPibeComoVos, daleMechaALaLluvia], efectoPreferido = remixear}

teAcordas :: Cancion
teAcordas = UnaCancion{titulo = "Te Acordas" , genero = "" , duracion = 100}
unPibeComoVos :: Cancion
unPibeComoVos = UnaCancion{titulo = "Te Acordas" , genero = "" , duracion = 100}
daleMechaALaLluvia :: Cancion
daleMechaALaLluvia = UnaCancion{titulo = "Dale Mecha a la Lluvia" , genero = "" , duracion = 100}



------------------------------------------------Parte B------------------------------------------------
vistazo :: Artista -> [Cancion]
vistazo unArtista = primeras3CancionesCortas unArtista

primeras3CancionesCortas :: Artista -> [Cancion]
primeras3CancionesCortas unArtista = (take 3) . filter esCancionCorta $ (canciones unArtista)  

esCancionCorta :: Cancion -> Bool
esCancionCorta unaCancion = ((>)150) . duracion $ unaCancion

playlist :: Genero -> [Artista] -> [Cancion]
playlist unGenero unaListaDeArtistas = filter (esDeGenero unGenero) (concatMap canciones unaListaDeArtistas)

esDeGenero :: Genero -> Cancion -> Bool
esDeGenero unGenero unaCancion = ((==) unGenero) . genero $ unaCancion 


--Parte C
hacerseDj :: Efecto -> Artista -> Artista
hacerseDj unEfecto unArtista = modificarCanciones (map (flip aplicarEfecto unEfecto)) unArtista

modificarCanciones :: ([Cancion] -> [Cancion]) -> Artista -> Artista
modificarCanciones unaFuncion unArtista = unArtista{canciones = unaFuncion.canciones $ unArtista}

tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo unArtista = all (esDeGenero (genero . primeraCancion $ unArtista)) (canciones unArtista)

primeraCancion :: Artista -> Cancion
primeraCancion unArtista = head . canciones $ unArtista

formarBanda :: String -> [Artista] -> Artista
formarBanda unNombre unosArtistas = UnArtista{nombre = unNombre, canciones = concatCanciones unosArtistas, efectoPreferido = metaEfecto (map efectoPreferido unosArtistas)}

concatCanciones :: [Artista] -> [Cancion]
concatCanciones unosArtistas = concatMap canciones unosArtistas   


obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva unArtista = UnaCancion{titulo = concatTitulos unArtista, duracion = concatDuracion unArtista, genero = concatGeneros unArtista}

concatTitulos :: Artista -> String
concatTitulos unArtista = concatMap titulo (canciones unArtista)

concatDuracion :: Artista -> Int
concatDuracion unArtista = sum (map duracion (canciones unArtista))

concatGeneros :: Artista -> Genero
concatGeneros unArtista = (mejorGeneroDeTodos unArtista) ++ "progresivo"

mejorGeneroDeTodos :: Artista -> Genero
mejorGeneroDeTodos unArtista = foldl1 (mejorGenero) (map genero (canciones unArtista))

mejorGenero :: Genero -> Genero -> Genero
mejorGenero "rock" _ = "rock"
mejorGenero _ "rock" = "rock"
mejorGenero unGenero otroGenero
    | unGenero == "reggaeton" = otroGenero
    | otroGenero == "reggaeton" = unGenero
    | (length unGenero) > (length otroGenero) = unGenero
    | otherwise = otroGenero


{-Parte D
En la escena apareció una artista que está arrasando con todos los premios gracias a que tiene infinitas canciones, pero antes de agregar su música en Currify te preguntamos:

¿Puede esta nueva artista hacerse dj?
No, porque, al tener canciones infinitas, nunca va a poder aplicarle el efecto deseado a sus canciones (es decir, nunca va a terminar de aplicarle el efecto a cada una de sus infinitas canciones).

¿Podemos echar un vistazo a su música?
Sí, siempre y cuando tenga al menos 3 canciones que duren < 180 segundos, ya que como Haskell funciona con lazy evaluation, al encontrar las 3 canciones que cumplan esa condicion, va a dejar de buscar, y retornar esas 3.

¿Podrá crear una obra maestra progresiva?
No, porque no va a poder ni concatenar todos los títulos (porque son infinitos y nunca dejaría de sumarlos), no va a poder calcular la duracion de la cancion (ya que, de nuevo, son infinitas canciones, ergo, infinita duracion de las mismas) ni el mejor género (ya que por como funciona la función, va a comparar infinitamente dos en dos cúal es el mejor género).
-}