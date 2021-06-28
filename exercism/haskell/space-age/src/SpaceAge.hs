module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune


ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (earthSeconds * (planetConst planet))
    where
        planetConst Mercury = 0.2408467
        planetConst Venus   = 0.61519726
        planetConst Earth   = 1.0
        planetConst Mars    = 1.8808158
        planetConst Jupiter = 11.862615
        planetConst Saturn  = 29.447498
        planetConst Uranus  = 84.016846
        planetConst Neptune = 164.79132
        earthSeconds = 31557600
