data Pais = Pais {
    nombre :: String,
    ingreso :: Float,
    publicoActivo :: Float,
    privadoActivo :: Float,
    riqueza :: [String],
    deuda :: Float
} deriving(Show, Eq)

namibia :: Pais
namibia = Pais {
    nombre = "Namibia",
    ingreso = 4140,
    publicoActivo = 400000,
    privadoActivo = 650000,
    riqueza = ["mineria", "ecoturismo"],
    deuda = 50
}

type Estrategia = Pais -> Pais


e1 :: Float -> Estrategia 
e1 = prestar

prestar :: Float -> Estrategia
prestar n pais = pais{deuda = deuda pais + n * 1.5}

e2 :: Int -> Estrategia
e2 x = reducirPublico x . reducirIngreso x

reducirPublico :: Int -> Estrategia
reducirPublico x pais = pais{publicoActivo = publicoActivo pais - x}

reducirIngreso :: Int -> Estrategia
reducirIngreso x pais
    |x > 100 = pais {ingreso = ingreso pais * 0.8}
    |otherwise = pais { ingreso = ingreso pais * 0.85}

e3 :: String -> Estrategia
e3 recurso = disminuir2MDeuda . quitarRecurso recurso

disminuir2MDeuda :: Estrategia 
disminuir2MDeuda pais = pais {deuda = deuda pais - 2}

quitarRecurso :: String -> Estrategia
quitarRecurso recurso pais = pais {riqueza = filter (/= recurso) (riqueza pais) }


e4 :: Estrategia
e4 pais = prestar (pBI pais) . reducirPublico 500 $ pais 

pBI :: Pais -> Float
pBI pais = ingreso pais * (publicoActivo pais + privadoActivo pais)