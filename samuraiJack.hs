data Elemento = UnElemento { 
    tipo :: String,
    ataque :: Personaje -> Personaje,
    defensa :: Personaje -> Personaje 
    }

data Personaje = UnPersonaje { 
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int 
    }


--1 
mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje {anioPresente = anio}

meditar :: Personaje -> Personaje
meditar personaje = personaje {salud = salud personaje * 1.5}

causarDanio :: Float -> Personaje -> Personaje
causarDanio cantidad personaje =  personaje { salud =  max 0 (salud personaje - cantidad)}

--2
esMalvado :: Personaje -> Bool
esMalvado  =  estaMaldad . listaDeTipoDeElementos 
listaDeTipoDeElementos :: Personaje -> [String]
listaDeTipoDeElementos personaje = map tipo $ elementos personaje
estaMaldad :: [String] -> Bool
estaMaldad = elem "Maldad"

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = salud personaje - (salud . ataque elemento) personaje

--durisimo

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (esEnemigoMortal personaje) enemigos

esEnemigoMortal :: Personaje -> Personaje -> Bool
esEnemigoMortal personaje enemigo = any (tieneAtaqueMortal personaje) . elementos $ enemigo

tieneAtaqueMortal :: Personaje -> Elemento -> Bool
tieneAtaqueMortal personaje elemento = danioQueProduce personaje elemento == salud personaje








