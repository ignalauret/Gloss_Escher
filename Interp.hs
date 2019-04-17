module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo
-- Agrego color a la FloatingPic
type FloatingPic = Color -> Vector -> Vector -> Vector -> Picture
type Output a = a -> FloatingPic


-- el vector nulo
zero :: Vector
zero = (0,0)

half :: Vector -> Vector
half = (0.5 V.*)

-- comprender esta función es un buen ejericio.
hlines :: Vector -> Float -> Float -> [Picture]
hlines v@(x,y) mag sep = map (hline . (*sep)) [0..]
  where hline h = line [(x,y+h),(x+mag,y+h)]

-- Una grilla de n líneas, comenzando en v con una separación de sep y
-- una longitud de l (usamos composición para no aplicar este
-- argumento)
grid :: Int -> Vector -> Float -> Float -> Picture
grid n v sep l = pictures [ls,translate 0 (l*toEnum n) (rotate 90 ls)]
  where ls = pictures $ take (n+1) $ hlines v sep l

-- Figuras adaptables comunes de colores
-- Agrego figura vacia
fVacia :: FloatingPic
fVacia _ _ _ _ = blank

trian1 :: FloatingPic
trian1 co a b c  = color co $ line $ map (a V.+) [zero, half b V.+ c , b , zero]

trian2 :: FloatingPic
trian2 co a b c = color co $ line $ map (a V.+) [zero, c, b,zero]

trianD :: FloatingPic
trianD co a b c = color co $ line $ map (a V.+) [c, half b , b V.+ c , c]

rectan :: FloatingPic
rectan co a b c = color co $ line [a, a V.+ b, a V.+ b V.+ c, a V.+ c,a]

simple :: Picture -> FloatingPic
simple p _ _ _ _ = p

fShape :: FloatingPic
fShape co a b c = color co $ line . map (a V.+) $ [ zero,uX, p13, p33, p33 V.+ uY , p13 V.+ uY
                 , uX V.+ 4 V.* uY ,uX V.+ 5 V.* uY, x4 V.+ y5
                 , x4 V.+ 6 V.* uY, 6 V.* uY, zero]
  where p33 = 3 V.* (uX V.+ uY)
        p13 = uX V.+ 3 V.* uY
        x4 = 4 V.* uX
        y5 = 5 V.* uY
        uX = (1/6) V.* b
        uY = (1/6) V.* c

-- Dada una función que produce una figura a partir de un a y un vector
-- producimos una figura flotante aplicando las transformaciones
-- necesarias. Útil si queremos usar figuras que vienen de archivos bmp.
transf :: (a -> Vector -> Picture) -> a -> Vector -> FloatingPic
transf f d (xs,ys) co a b c  = color co $ translate (fst a') (snd a') .
                             scale (magV b/xs) (magV c/ys) .
                             rotate ang $ f d (xs,ys)
  where ang = radToDeg $ argV b
        a' = a V.+ half (b V.+ c)

-- Intepreta Rotar90(Dibujo a)
intRotar90 :: FloatingPic -> FloatingPic
intRotar90 fp c x y z = fp c (x V.+ y) z (V.negate y)

-- Intepreta Rotar45(Dibujo a)
intRotar45 :: FloatingPic -> FloatingPic
intRotar45 fp c x y z = fp c (x V.+ half(y V.+ z)) (half (y V.+ z)) (half (z V.- y))

-- Intepreta Espejar(Dibujo a)
intEspejar :: FloatingPic -> FloatingPic
intEspejar fp c x y z = fp c (x V.+ y) (V.negate y) z

-- Intepreta Encimar (Dibujo a) (Dibujo a)
intEncimar :: FloatingPic -> FloatingPic -> FloatingPic
intEncimar fp1 fp2 c x y z = pictures[fp1 c x y z, fp2 c x y z]

-- Intepreta Juntar Int Int (Dibujo a) (Dibujo a)
intJuntar :: Int -> Int -> FloatingPic -> FloatingPic  -> FloatingPic
intJuntar a b fp1 fp2 c x y z = pictures[fp1 c x y' z, fp2 c (x V.+ y') (r' V.* y) z]
      where r' = n/(m + n)
            r = m/(m + n)
            y' = r V.* y
            m = intToFloat(a)
            n = intToFloat(b)

-- Intepreta Apilar Int Int (Dibujo a) (Dibujo a)
intApilar :: Int -> Int -> FloatingPic -> FloatingPic -> FloatingPic
intApilar a b fp1 fp2 c x y z = pictures[fp1 c (x V.+ z') y (r V.* z), fp2 c x y z']
      where r' = n/(m + n)
            r = m/(m + n)
            z' = r' V.* z
            m = intToFloat(a)
            n = intToFloat(b)

--Interpreta un Dibujo coloreando cada Basica de su color (Ver definicion de Dibujo)
interp :: Output a -> Output (Dibujo a)
interp f Vacio = fVacia
interp f (Basica d c ) = paint c $ f d
interp f (Rotar90 d) = intRotar90 (interp f d)
interp f (Espejar d) = intEspejar (interp f d)
interp f (Rotar45 d) = intRotar45 (interp f d)
interp f (Apilar n m d1 d2) = intApilar n m (interp f d1) (interp f d2)
interp f (Juntar n m d1 d2) = intJuntar n m (interp f d1) (interp f d2)
interp f (Encimar d1 d2) = intEncimar (interp f d1) (interp f d2)

-- Convierte Int a Float
intToFloat :: Int -> Float
intToFloat x = fromInteger(toInteger(x))

-- Devuelve la FloatingPic pintada del color $c 
paint :: Color -> FloatingPic -> FloatingPic
paint co fp c x y z = fp co x y z
