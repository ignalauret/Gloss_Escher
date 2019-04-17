module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.UI.GLUT.Begin
import Dibujo
import Interp
import Escher
import qualified Basico.Ejemplo as E

data Conf a = Conf {
    basic :: Output a
  , fig  :: Dibujo a
  , width :: Float
  , height :: Float
  }

ej x y = Conf {
                basic = E.interpBas
              , fig = E.ejemp
              , width = x
              , height = y
              }

 ------ Para ibujar del ghci -----

 {- Interpretador de figuras para introducir por consola
    interpBas 1 = trian1
    interpBas 2 = trian2
    interpBas 3 = rectan
    interpBas 4 = trianD
-}

 -- Crea un Conf
crear :: Float -> Float -> Dibujo E.Basica -> Conf E.Basica
crear x y d = Conf{ basic = E.interpBas, fig = d, width = x, height = y}

-- Creo el Conf y llamo a initial.
-- Para dibujar por terminal, abro el ghci y pongo: dibujar ancho alto Dibujo
dibujar :: Float -> Float -> Dibujo E.Basica -> IO ()
dibujar x y d = initial $ return (crear x y (noRot360 $ noFlip2 d))


-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: IO (Conf E.Basica) -> IO ()
initial cf = cf >>= \cfg ->
                  let x  = width cfg
                      y  = height cfg
                  -- Comento la grid y agrego correctores de la figura 
                  in display win white {- . withGrid -} $ interp (basic cfg) (noRot360 $ noFlip2 $ fig cfg) red (0,0) (x,0) (0,y)
  where --withGrid p = pictures [p, color grey $ grid 10 (0,0) 100 10]
        grey = makeColorI 120 120 120 120

win = InWindow "Nice Window" (200, 200) (0, 0)
main = initial $ return (ej 100 100)
