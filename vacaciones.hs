data Turista = UnTurista{ nivelDeCansancio :: Int , nivelDeStress :: Int , viajaSolo :: Bool , idiomasQueHabla ::[String] } deriving (Show)

--Turistas--
ana :: Turista
ana = UnTurista{nivelDeCansancio = 0 , nivelDeStress = 21 , viajaSolo = False , idiomasQueHabla = ["Español"]} 

beto :: Turista
beto = UnTurista{nivelDeCansancio = 15 , nivelDeStress = 15 , viajaSolo = True , idiomasQueHabla = ["Alemán"]}

cathi :: Turista
cathi = UnTurista{nivelDeCansancio = 15 , nivelDeStress = 15 , viajaSolo = True , idiomasQueHabla = ["Alemán" , "Catalán"]}
--Turistas--


--FuncionesMap--
mapNivelDeCansancio :: (Int -> Int) -> Turista -> Turista
mapNivelDeCansancio unaFuncion unTurista = unTurista{ nivelDeCansancio = unaFuncion.nivelDeCansancio $ unTurista }

mapNivelDeStress :: (Int -> Int) -> Turista -> Turista
mapNivelDeStress unaFuncion unTurista = unTurista{ nivelDeStress = unaFuncion.nivelDeStress $ unTurista }

mapViajaSolo :: (Bool -> Bool) -> Turista -> Turista
mapViajaSolo unaFuncion unTurista = unTurista{ viajaSolo = unaFuncion.viajaSolo $ unTurista }

mapIdiomasQueHabla :: ([String] -> [String]) -> Turista -> Turista
mapIdiomasQueHabla unaFuncion unTurista = unTurista{ idiomasQueHabla = unaFuncion.idiomasQueHabla $ unTurista }
--FuncionesMap--

--Excursiones--
type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya unTurista
    | viajaSolo unTurista = mapNivelDeCansancio (subtract 5) unTurista
    | otherwise = mapNivelDeCansancio (subtract 1) unTurista

apreciarElementoDelPaisaje :: String -> Excursion
apreciarElementoDelPaisaje unPaisaje unTurista = mapNivelDeStress (subtract.length $ unPaisaje) unTurista

salirHablarUnIdioma :: String -> Excursion
salirHablarUnIdioma unIdioma unTurista = mapIdiomasQueHabla ((:) unIdioma) . mapViajaSolo (const False) $ unTurista

salirACaminar :: Int -> Excursion
salirACaminar unosMinutos unTurista = mapNivelDeCansancio ( + (div unosMinutos 4)) . mapNivelDeStress ( subtract (div unosMinutos 4)) $ unTurista

paseoEnBarco :: String -> Excursion
paseoEnBarco "fuerte" unTurista = mapNivelDeStress ( + 6) . mapNivelDeCansancio (+ 10) $ unTurista
paseoEnBarco "moderada" unTurista = unTurista
paseoEnBarco "tranquila" unTurista = salirACaminar 10 . apreciarElementoDelPaisaje "mar" . salirHablarUnIdioma "aleman" $ unTurista
paseoEnBarco _ unTurista = unTurista
--Excursiones--

--2)a--
hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion unaExcursion unTurista   = reducirStress10Porciento . unaExcursion $ unTurista

reducirStress10Porciento:: Turista -> Turista
reducirStress10Porciento unTurista = mapNivelDeStress ( subtract (div (nivelDeStress unTurista) 10)) unTurista
--2)a--

--2)b--
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun unIndice unTurista unaExcursion = deltaSegun unIndice (hacerExcursion unaExcursion unTurista) unTurista
--2)b--

--2)c--
esExcursionEducativa :: Turista ->  Excursion -> Bool
esExcursionEducativa unTurista unaExcursion = (deltaExcursionSegun (\unTurista -> length.idiomasQueHabla $ unTurista) unTurista unaExcursion) >= 1

esExcursionDesestresante ::  Turista -> Excursion -> Bool
esExcursionDesestresante unTurista unaExcursion  = (deltaExcursionSegun (\unTurista -> nivelDeStress unTurista) unTurista unaExcursion) >= 3

--3)--

--PaqueteDeExcursion--
type PaqueteDeExcursion = [Excursion]
completo :: PaqueteDeExcursion
completo = [salirACaminar 20, apreciarElementoDelPaisaje "cascada", salirACaminar 40 , irALaPlaya , salirHablarUnIdioma "melmacquiano"]

ladoB :: Excursion -> PaqueteDeExcursion
ladoB unaExcursion = [paseoEnBarco "tranquilo" , hacerExcursion unaExcursion , salirACaminar 120]

islaVecina :: String -> PaqueteDeExcursion
islaVecina unaMarea = [paseoEnBarco unaMarea , hacerExcursion (excursionSegunMarea unaMarea) , paseoEnBarco unaMarea]

excursionSegunMarea :: String -> Excursion
excursionSegunMarea "fuerte" = apreciarElementoDelPaisaje "lago"
excursionSegunMarea _ = irALaPlaya
--PaqueteDeExcursion--

hacerUnTour :: Turista -> PaqueteDeExcursion -> [Turista] 
hacerUnTour unTurista unPaqueteDeExcursion = map (hacerExcursion' (aumentarStressPorTamanioDelTour unTurista unPaqueteDeExcursion)) unPaqueteDeExcursion 

aumentarStressPorTamanioDelTour :: Turista -> PaqueteDeExcursion -> Turista
aumentarStressPorTamanioDelTour unTurista unPaqueteDeExcursion = mapNivelDeStress (+ (length unPaqueteDeExcursion)) unTurista
hacerExcursion':: Turista -> Excursion -> Turista
hacerExcursion' unTurista unaExcursion = unaExcursion unTurista


--3)b--

existeTourConvincente:: [PaqueteDeExcursion] -> Turista -> Bool
existeTourConvincente unaListaDeTours unTurista = any (esTourConvincente unTurista) unaListaDeTours

esTourConvincente :: Turista -> PaqueteDeExcursion -> Bool
esTourConvincente unTurista unaPaqueteDeExcursion = any ((esExcursionDesestresanteyDejaAcompaniado unTurista)) unaPaqueteDeExcursion

esExcursionDesestresanteyDejaAcompaniado :: Turista -> Excursion -> Bool
esExcursionDesestresanteyDejaAcompaniado unTurista unaExcursion = (esExcursionDesestresante unTurista unaExcursion ) && (dejaAcompaniado unTurista unaExcursion )

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado unTurista unaExcursion = (viajaSolo.unaExcursion $ unTurista) == False

efectividadDeUnTour :: [Turista] -> Excursion -> Int
efectividadDeUnTour unGrupoDeTuristas unaExcursion = sum . map (esperitualidadRecibida unaExcursion) $ unGrupoDeTuristas

esperitualidadRecibida :: Excursion -> Turista -> Int
esperitualidadRecibida unaExcursion unTurista = (deltaExcursionSegun (\unTurista -> nivelDeStress unTurista) unTurista unaExcursion ) + (deltaExcursionSegun (\unTurista -> nivelDeCansancio unTurista) unTurista unaExcursion )

{-4
a) 
tourInfinito :: PaqueteDeExcursion
tourInfinito = repeat irALaPlaya

b) esTourConvincente ana tourInfinito 
Para ambos casos, con hacer una vez la funcion esExcursionDesestresanteyDejaAcompaniado, ya sabremos si se puede saber si es convincente (ya que, al hacer la misma excursion
infinitamente, jamás va a cambiar el resultado)

Para Ana, al ir siempre a la playa, nunca va a cambiar si está acompañada o no, entonces siempre va a decir que está sola, por lo que nunca va a cumplir la segunda condicion.
Sin embargo, como Beto esta...

c) No, ya que la excursion es infinita entonces nunca va a 

-}


