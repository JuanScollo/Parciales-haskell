--1

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
}



data Gema = Gema {
    tipo :: String,
    poder :: Poder
}

type Poder = Personaje -> Personaje

data Personaje = Personaje {
    edad :: Int,
    energia :: Int,
    habilidades :: [Habilidad],
    nombre :: String,
    planeta :: Planeta
}

type Habilidad = String
type Planeta = String



data Universo = Universo{
    personajes :: [Personaje],
    energiaU :: Int
}

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo 
    | esGuanteleteCompleto guantelete = universo { personajes = take (numMitadDeUniverso . personajes $ universo) (personajes universo) }
    |otherwise = universo

numMitadDeUniverso :: [Personaje] -> Int
numMitadDeUniverso personajes = div (length personajes) 2

esGuanteleteCompleto :: Guantelete -> Bool
esGuanteleteCompleto guantelete = material guantelete == "uru" && (length . gemas) guantelete  == 6 

--2

aptoParaPendex :: Universo -> Bool
aptoParaPendex universo = any ((<45) . edad) (personajes universo)

energiaTotalCorrecta :: Universo -> Bool
energiaTotalCorrecta universo = energiaU universo == sumaDeEnergiaPersonajes universo

sumaDeEnergiaPersonajes :: Universo -> Int
sumaDeEnergiaPersonajes universo = sum (map energia . personajes $ universo)

--3

debilitarMente :: Int -> Poder
debilitarMente cantidad personaje = personaje { energia = max 0 (energia personaje - cantidad)}

controlarAlma :: Habilidad -> Poder
controlarAlma habilidad = debilitarMente 10 . quitarHabilidad habilidad
quitarHabilidad :: Habilidad -> Poder
quitarHabilidad habilidad personaje 
    |elem habilidad (habilidades personaje) = personaje {habilidades = filter ( /= habilidad) (habilidades personaje)}
    |otherwise = personaje


elEspacio :: Planeta -> Poder
elEspacio planeta = debilitarMente 20 . transportarA planeta
transportarA :: Planeta -> Poder
transportarA planeta personaje = personaje {planeta = planeta}

elPoder :: Poder
elPoder personaje = debilitarMente (energia personaje) . quitarSiHab2 $ personaje
quitarSiHab2 :: Poder
quitarSiHab2 personaje
    |length (habilidades personaje) < 3 = personaje { habilidades = []}
    |otherwise = personaje

elTiempo :: Poder
elTiempo = debilitarMente 50 . reducirAMitadEdad
reducirAMitadEdad :: Poder
reducirAMitadEdad personaje = personaje {edad = max 18 (div (edad personaje) 2 )}

gemaLoca :: Poder -> Poder
gemaLoca poder = poder . poder 
-- data Gema = Gema {
--     tipo :: String,
--     poder :: Poder
-- }deriving(Show, Eq)

-- type Poder = Personaje -> Personaje


