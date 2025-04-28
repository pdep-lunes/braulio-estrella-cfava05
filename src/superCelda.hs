data Personaje = Personaje {
    nombre :: String,
    basicPower :: String,
    superPower :: String,
    activePower :: Bool,
    cantidadDeVida :: Int
}

personajeEspina :: Personaje
personajeEspina = Personaje {
    nombre = "Espina",
    basicPower = "bolaEspinosa",
    superPower = "granadaDeEspinas",
    activePower = "granadaDeEspinas",
    cantidadDeVida = 4800
}

personajePamela :: Personaje
personajePamela = Personaje
{
    nombre = "Pamela",
    basicPower = "lluviaDeTuercas",
    superPower = "torretaCurativa",
    activePower = False,
    cantidadDeVida = 9600
}


vidaDePersonaje :: Personaje -> Int
vidaDePersonaje unPersonaje = cantidadDeVida unPersonaje

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje = unPersonaje 
{
cantidadDeVida = if cantidadDeVida unPersonaje <= 1000
    then 0
    else cantidadDeVida unPersonaje -1000
}

tuercasSanadoras :: Personaje -> Personaje
tuercasSanadoras unPersonaje = unPersonaje
{
    cantidadDeVida = cantidadDeVida unPersonaje +800
}

tuercasDaniñas :: Personaje ->Personaje
tuercasDaniñas unPersonaje = unPersonaje
{
    cantidadDeVida = cantidadDeVida unPersonaje / 2
}

agregarFrase :: Personaje -> Personaje
agregarFrase unPersonaje = unPersonaje
{
    nombre unPersonaje ++ "Espina estuvo aquí"
}

desactivarPoder :: Personaje -> Personaje
desactivarPoder unPersonaje = unPersonaje
{
    activePower = False
}

granadaDeEspinas :: Personaje -> Personaje
granadaDeEspinas radio unPersonaje
{
    radio > 3 && cantidadDeVida unPersonaje < 800 = desactivarPoder
    radio > 3 = agregarFrase unPersonaje
    radio > 3 cantidadDeVida = 0
}

torretaCurativa :: Personaje -> Personaje
torretaCurativa unPersonaje = unPersonaje
{
    activePower = True,
    cantidadDeVida = cantidadDeVida unPersonaje * 2
}

atacarContrincante :: Personaje -> Personaje -> Personaje
atacarContrincante atacante contrincante = basicPower atacante ++ superPower atacante contrincante
{
    superPower = True,
    basicPower = True
}

atacarConPoderEspecial :: Personaje -> Personaje -> Personaje
atacarConPoderEspecial atacante contrincante
{
   activePower atacante = atacarContrincante atacante contrincante
   otherwise = contrincante
}