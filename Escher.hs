module Escher where
import Graphics.Gloss
import Dibujo

-- Tipo usado por escher (No se porque usa bool)
type Escher = Bool

-- Dibujo t de escher
dibujo_t :: Dibujo Escher -> Dibujo Escher
dibujo_t p = Encimar p $ Encimar p2 p3
           where p2 = Espejar $ Rotar45 p
                 p3 = Rotar90 $ Rotar90 $ Rotar90 p2

-- Dibujo u de escher
dibujo_u :: Dibujo Escher -> Dibujo Escher
dibujo_u p = Encimar (Encimar p2
                              (Rotar90 p2))
                     (Encimar (Rotar90 $ Rotar90 p2)
                              (Rotar90 $ Rotar90 $ Rotar90 p2))
            where p2 = Espejar $ Rotar45 p

-- Side de escher
side :: Int -> Dibujo Escher
side 0 = Vacio
side n = cuarteto (side $ n-1)
                  (side $ n-1)
                  (Rotar90 $ dibujo_t $ Basica True blue)
                  (dibujo_t $ Basica True red)

-- Esquina de escher
corner :: Int -> Dibujo Escher
corner 0 = Vacio
corner n = cuarteto (corner $ n-1)
                    (side $ n-1)
                    (Rotar90 $ side $ n-1)
                    (dibujo_u $ Basica True violet)

-- Noneto para poner las 4 esquinas, 4 lados y centro de escher
nonet p q r s t u v w x = Apilar 1 2 (Juntar 1 2 p $ Juntar 1 1 q r)
                                     (Apilar 1 1 (Juntar 1 2 s $ Juntar 1 1 t u)
                                                 (Juntar 1 2 v $ Juntar 1 1 w x))

-- Funcion escher que llama a noneto de lo necesario
-- escher n -> escher con n iteraciones
escher :: Int -> Dibujo Escher
escher n = nonet (corner n)
                 (side n)
                 (Rotar90 $ Rotar90 $ Rotar90 $corner n)
                 (Rotar90 $ side n)
                 (dibujo_u $ Basica True black)
                 (Rotar90 $ Rotar90 $ Rotar90 $ side n)
                 (Rotar90 $ corner n)
                 (Rotar90 $ Rotar90 $ side n)
                 (Rotar90 $ Rotar90 $ corner n)
