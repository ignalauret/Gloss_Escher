module Basico.Ejemplo where
import Graphics.Gloss
import Dibujo
import Interp
import Escher

type Basica = Escher
-- Figura de ejemplo para Main 
ejemp :: Dibujo Basica
ejemp = escher 3


-- Valores arbitrarios de la interpretacion de las figuras
interpBas :: Output Basica
-- Para Escher
interpBas True = trian2

-- Para Dibujos
{-
interpBas 1 = trian1
interpBas 2 = trian2
interpBas 3 = rectan
interpBas 4 = trianD
-}
