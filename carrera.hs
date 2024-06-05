import Data.List (find)

data Auto = Auto{
    color :: String,
    velocidad :: Int,
    distancia :: Int,
    estaParticipando :: Bool
}deriving(Show, Eq)

type Carrera = [Auto]

carrera :: Carrera
carrera = [rojo, azul, blanco]


-----------
--- 1 a ---
-----------

estaCercaDe :: Auto -> Auto -> Bool
estaCercaDe auto1 auto2 = sonDiferentes auto1 auto2 && abs (distancia auto1 - distancia auto2) < 10

sonDiferentes :: Auto -> Auto -> Bool
sonDiferentes auto1 auto2 = color auto1 /= color auto2

-----------
--- 1 b ---
-----------

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera =  not $ any (estaCercaDe auto) carrera && vaGanando auto carrera

vaGanando :: Auto -> Carrera -> Bool
vaGanando auto = all ((distancia auto >) . distancia) 

-----------
--- 1 c ---
-----------
puesto :: Auto -> Carrera -> Int
puesto auto1 carrera = 1 + length (filter (vaAtras auto1) carrera)

vaAtras :: Auto -> Auto -> Bool
vaAtras auto1 auto2 = distancia auto1 < distancia auto2  

-----------
--- 2 a ---
-----------
corra :: Int -> Auto -> Auto
corra horas auto = auto{distancia = distancia auto + velocidad auto * horas}

-------------
--- 2 b.i ---
-------------
type Modificador = Int -> Int

alterarVelocidad :: Modificador -> Auto -> Auto
alterarVelocidad mod auto = auto {velocidad = max 0 (mod (velocidad auto))}

bajarLaVelocidad :: Int -> Auto -> Auto
bajarLaVelocidad cantidad = alterarVelocidad (subtract cantidad) 

-- restarCantidad :: Int -> Int -> Int
-- restarCantidad cantidad velocidad = velocidad - cantidad

-------------
---  3  -----
-------------
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

type PowerUp = Auto -> Carrera -> Carrera

terremoto :: PowerUp
terremoto auto = afectarALosQueCumplen (estaCercaDe auto) (bajarLaVelocidad 50)

miguelitos :: Int -> PowerUp
miguelitos cantidad auto = afectarALosQueCumplen (esGanadorRespectoDe auto) (bajarLaVelocidad cantidad)

esGanadorRespectoDe :: Auto -> Auto -> Bool
esGanadorRespectoDe autoRef otroAuto = distancia autoRef < distancia otroAuto


-- jetpack :: Int -> PowerUp
-- jetpack duracion auto = afectarALosQueCumplen (== auto) (corra duracion . usarJetpack $ auto)
-- no se porque no funciona

jetpack :: Int -> PowerUp
jetpack duracion auto = afectarALosQueCumplen (== auto) (aplicarJetpack duracion)


 -- hice la funcion auxiliar para poder pasar solamente la funcion y duracion en afectaralosquecumplen
aplicarJetpack :: Int -> Auto -> Auto
aplicarJetpack duracion = corra duracion . usarJetpack

usarJetpack :: Auto -> Auto
usarJetpack auto = auto{ velocidad = 2 * velocidad auto}

-------------
---  4  -----
-------------

type Eventos = Carrera -> Carrera

type TablaDePosiciones = [(Int,Color)]
type Color = String


simularCarrera :: Carrera -> [Eventos] -> [(Int, Color)]
simularCarrera carrera = hacerTablaDePosiciones . estadoFinalCarrera carrera

hacerTablaDePosiciones :: Carrera -> TablaDePosiciones
hacerTablaDePosiciones carrera = zip (puestosDeCarrera carrera) (coloresDeAutos carrera)

puestosDeCarrera :: Carrera -> [Int]
puestosDeCarrera carrera = map (flip puesto carrera) carrera

coloresDeAutos :: Carrera -> [String]
coloresDeAutos = map color

estadoFinalCarrera :: Carrera -> [Eventos] -> Carrera
estadoFinalCarrera carrera eventos = foldl (flip ($)) carrera eventos

correnTodos :: Int -> Carrera -> Carrera
correnTodos duracion = map (corra duracion)

usarPowerUp :: PowerUp -> Color -> Eventos
usarPowerUp powerUp colorBuscado carrera = powerUp (buscarAutoPorColor colorBuscado carrera) carrera


buscarAutoPorColor :: Color -> Carrera -> Auto
buscarAutoPorColor colorBuscado carrera =  head . filter ((== colorBuscado) . color) $ carrera






--------------------
--- AUTOS PRUEBA ---
--------------------

rojo :: Auto
rojo = Auto{
    color = "rojo",
    velocidad = 120,
    distancia = 0,
    estaParticipando = True
}

azul :: Auto
azul = Auto{
    color = "azul",
    velocidad = 120,
    distancia = 0,
    estaParticipando = True
}

blanco :: Auto
blanco = Auto{
    color = "blanco",
    velocidad = 120,
    distancia = 0,
    estaParticipando = True
}