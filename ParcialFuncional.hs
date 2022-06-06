import Text.Show.Functions

data Participante = UnParticipante{
    nombre :: String,
    trucos :: [Truco],
    especialidad :: Plato
}deriving(Show)

data Plato = UnPlato{
    dificultad :: Int,
    componentes :: [Componente]
}deriving(Show)

type Componente = (String , Int)

type Truco = Plato -> Plato
--Maps--
mapComponentes ::  ([(String ,Int)] -> [(String , Int)]) -> Plato -> Plato
mapComponentes  unaFuncion unPlato = unPlato{componentes = unaFuncion.componentes $ unPlato}


--Parte A-- Trucos
agregarIngrediente :: Componente -> Plato -> Plato
agregarIngrediente unComponente unPlato = mapComponentes ((:) unComponente) unPlato 


endulzar :: Int -> Truco
endulzar unaCantidadDeAzucar unPlato = agregarIngrediente ("Azucar" , unaCantidadDeAzucar) unPlato

salar :: Int -> Truco
salar unaCantidadDeSal unPlato = agregarIngrediente ("Sal" , unaCantidadDeSal) unPlato

darSabor :: Int -> Int -> Truco
darSabor unaCantidadDeAzucar unaCantidadDeSal unPlato = salar unaCantidadDeSal . endulzar unaCantidadDeAzucar $ unPlato

duplicarPorcion :: Truco 
duplicarPorcion unPlato = mapComponentes (map modificarCantidad) unPlato

modificarCantidad :: (String , Int) -> (String , Int)
modificarCantidad (unIngrediente , unaCantidad) = (unIngrediente , unaCantidad * 2)

simplificar :: Truco
simplificar unPlato
    |(tieneDificultadMayor 7 unPlato ) || (tieneMasDe5Componentes unPlato) = cambiarDificultad 5 . mapComponentes (filter esMayor5LosGramos) $ unPlato
    |otherwise = unPlato


cambiarDificultad :: Int -> Plato -> Plato
cambiarDificultad unaDificultad unPlato = unPlato{dificultad = unaDificultad}
esMayor5LosGramos :: Componente -> Bool
esMayor5LosGramos (_ , unaCantidad) = unaCantidad > 5
tieneDificultadMayor :: Int -> Plato -> Bool
tieneDificultadMayor unNumero unPlato = unNumero < (dificultad unPlato) 
tieneMasDe5Componentes :: Plato -> Bool
tieneMasDe5Componentes unPlato = (length . componentes $ unPlato) > 5


--De los platos tambien nos interesa saber
esVegano :: Plato -> Bool
esVegano unPlato = not (tieneIngrediente "Carne" unPlato || tieneIngrediente "Huevos" unPlato || tieneIngrediente "Alimentos lácteos" unPlato)

tieneIngrediente :: String -> Plato -> Bool
tieneIngrediente unIngrediente unPlato = elem unIngrediente (map fst (componentes unPlato))

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not . tieneIngrediente "Harina" $ unPlato

esComplejo :: Plato -> Bool
esComplejo unPlato = tieneMasDe5Componentes unPlato && tieneDificultadMayor 7 unPlato

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = tieneMasDe2GramosDeSal unPlato
    
tieneMasDe2GramosDeSal :: Plato -> Bool
tieneMasDe2GramosDeSal unPlato = (sum . map snd . filter esSal . componentes $ unPlato) > 2
esSal :: Componente -> Bool
esSal ("Sal",_) = True
esSal (_,_) = False

--Parte B--
pepe :: Participante   
pepe = UnParticipante{
    nombre = "Pepe Ronccino",
    trucos = [darSabor 2 5, simplificar, duplicarPorcion],
    especialidad = platoPepe 
}

platoPepe :: Plato
platoPepe = UnPlato{
    dificultad = 5,
    componentes = [("Sal",10),("Morrones",20),("Pollo",100),("Azucar",100),("Pimienta",10),("Ingrediente Secerto",1)]
}


--Parte C--
cocinar :: Participante -> Plato
cocinar unParticipante = foldl aplicarTruco (especialidad unParticipante) (trucos unParticipante)

aplicarTruco :: Plato -> Truco -> Plato
aplicarTruco unPlato unTruco = unTruco unPlato

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = (dificultad unPlato > dificultad otroPlato) && (sumaComponentes unPlato < sumaComponentes otroPlato)

sumaComponentes :: Plato -> Int
sumaComponentes unPlato = sum . map snd . componentes $ unPlato

devolverMejorParticipante :: Participante -> Participante -> Participante
devolverMejorParticipante unParticipante otroParticipante
    |esMejorQue (cocinar unParticipante) (cocinar otroParticipante) = unParticipante
    |otherwise = otroParticipante

participanteEstrella :: [Participante] -> Participante
participanteEstrella [participante] = participante
participanteEstrella (participante1:participante2:colaParticipantes) = devolverMejorParticipante participante1 (participanteEstrella (participante2:colaParticipantes))




platinum :: Plato
platinum = UnPlato{
    dificultad = 10,
    componentes = componentesInfinitas
}

componentesInfinitas :: [Componente]
componentesInfinitas = map hacerComponente [1..]

hacerComponente :: Int -> (String ,Int) 
hacerComponente unNumero = ("Ingrediente " ++ (show unNumero) , unNumero)

{- Parte D 
¿Qué sucede si aplicamos cada uno de los trucos modelados en la Parte A al platinum?
-Con endulzar, salar y darSabor no va a haber problema porque la funcion (:), utilizada para estas 3 funciones,
ingresa un elemento al inicio de la lista, entonces va a ingresarlo al inicio de la lista sin problemas.
-Con duplicarPorcion y simplificar, no va a funcionar. Para el primer caso, va a duplicar infinitamente, ya que hay infinitos componentes, pero nunca va a terminar.
Con simplificar, va a pasar algo similar, como usa filter, va a ir componente por componente fijandose si cumple la condicion dada, pero nunca va a temrinar de fijarse.

¿Cuáles de las preguntas de la Parte A (esVegano, esSinTacc, etc.) se pueden responder sobre el platinum? 
-Ambos si es vegano o si es sin tacc no se van a poder responder ya que va a estar comparando todos los componentes con los que se le pida (sal, carne, etc.)
y nunca va a parar de buscar, y sabemos que no los va a encontrar porque esos elementos no se encuentran en platinum.
-Es complejo tampoco porque nunca va a poder determinar el tamaño total de los componentes para definir si es mayor a 5. 
-Por último, noAptoHipertension tampoco funciona, ya que la funcion "tieneMasDe2GramosDeSal" no va a ni siquiera filtrar las componentes que sean sal, ya que al ser
infinitas, y por como funciona filter, nunca va a parar hasta que termine de leer toda la lista infinita

¿Se puede saber si el platinum es mejor que otro plato?
No, porque cuando intente hacer la sumaComponentes del plato, nunca va a poder terminar de sumar todos los componentes para poder compararlo con el otro plato.
-}