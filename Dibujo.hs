module Dibujo where
import Graphics.Gloss

-- Estructura Dinujo
data Dibujo a =
    Vacio
  | Basica a Color
  | Rotar90 (Dibujo a)
  | Espejar (Dibujo a)
  | Rotar45 (Dibujo a)
  | Apilar Int Int (Dibujo a) (Dibujo a)
  | Juntar Int Int (Dibujo a) (Dibujo a)
  | Encimar (Dibujo a) (Dibujo a)
  deriving (Eq,Show)

------------------------ Constructores --------------------------
vaciar :: a -> Dibujo a
vaciar d = Vacio

r90 :: Dibujo a -> Dibujo a
r90 x = Rotar90 x

espejar :: Dibujo a -> Dibujo a
espejar x = Espejar x

r45 :: Dibujo a -> Dibujo a
r45 x = Rotar45 x

apilar :: Int -> Int -> Dibujo a -> Dibujo a -> Dibujo a
apilar n m d1 d2 = Apilar n m d1 d2

juntar :: Int -> Int -> Dibujo a -> Dibujo a -> Dibujo a
juntar n m d1 d2 = Juntar n m d1 d2

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar d1 d2 = Encimar d1 d2

------------------------ Combinadores -----------------------------
  -- composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f 0 x = x
comp f n x = comp f (n-1) (f(x))

  -- rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 d = comp r90 2 d

r270 :: Dibujo a -> Dibujo a
r270 d = comp r90 3 d

  -- Pone una figura sobre la otra, ambas ocupan el mismo espacio
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) d1 d2 = apilar 100 100 d1 d2

  -- Pone una figura al lado de la otra, ambas ocupan el mismo espacio
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) d1 d2 = juntar 100 100 d1 d2

  -- Superpone una figura con otra
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) d1 d2 = encimar d1 d2

-- dada una figura la repite en cuatro cuadrantes
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto d1 d2 d3 d4 = (.-.)((///) d1 d2) ((///) d3 d4)

  -- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 d = (^^^)((^^^) d (r90 d)) ((^^^)(r180 d) (r270 d))

  -- cuadrado con la misma figura rotada $i$ por $90$ para $i \in \{1..3\}$.
  -- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar d = (.-.) ((///) d (r90 d)) (r180 ((///) d (r90 d)))

-------------------------- Esquemas para Manipulacion -------------------------

  -- ver un a como una figura
pureDib :: a -> Color ->Dibujo a
pureDib x c = Basica x c

  -- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f Vacio = Vacio;
mapDib f (Basica a c ) = pureDib (f a) c
mapDib f (Rotar90 a) = r90 (mapDib f a)
mapDib f (Espejar a) = espejar (mapDib f a)
mapDib f (Rotar45 a) = r45 (mapDib f a)
mapDib f (Apilar n m a b ) = apilar n m (mapDib f a) (mapDib f b)
mapDib f (Juntar n m a b ) = juntar n m (mapDib f a) (mapDib f b)
mapDib f (Encimar a b ) = encimar (mapDib f a) (mapDib f b)

  -- verificar que las operaciones satisfagan
  -- 1. mapDib id = id
  -- 2. mapDib (g . f) = mapDib g . mapDib f

  --
cambia :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
cambia f Vacio = Vacio;
cambia f (Basica a _ ) = (f a)
cambia f (Rotar90 a) = r90 (cambia f a)
cambia f (Espejar a) = espejar (cambia f a)
cambia f (Rotar45 a) = r45 (cambia f a)
cambia f (Apilar n m a b) = apilar n m (cambia f a) (cambia f b)
cambia f (Juntar n m a b) = juntar n m (cambia f a) (cambia f b)
cambia f (Encimar a b) = encimar (cambia f a) (cambia f b)

  -- convencerse que se satisface
  -- 1. cambiar pureDibe = id
  -- 2. cambiar f (pureDibe a) = f a
  -- 3. (cambiar g) (cambiar f ma) = cambiar (cambiar g . f) ma

  -- estructura general para la semántica (a no asustarse. Ayuda:
  -- pensar en foldr y las definiciones de intro a la lógica)

sem :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
         (Int -> Int -> b ->  b ->  b) ->
         (Int -> Int -> b ->  b ->  b) ->
         (b -> b -> b) ->
         Dibujo a -> b

-- f es la funcion que se aplica a la  Basica
-- r90 es la funcion que se le aplica a Rotar90
-- es es la funcion que se le aplica a Espejar
-- r45 es la funcion que se le aplica a Rotar45
-- a es la funcion que se le aplica a Apilar
-- j es la funcion que se le aplica a Juntar
-- en es la funcion que se le aplica a Encimar
sem f r90 es r45 a j en (Basica d _ ) = f d
sem f r90 es r45 a j en (Rotar90 d) = r90 (sem f r90 es r45 a j en d)
sem f r90 es r45 a j en (Espejar d) = es (sem f r90 es r45 a j en d)
sem f r90 es r45 a j en (Rotar45 d) = r45 (sem f r90 es r45 a j en d)
sem f r90 es r45 a j en (Apilar n m d1 d2) = a n m (sem f r90 es r45 a j en d1) (sem f r90 es r45 a j en d2)
sem f r90 es r45 a j en (Juntar n m d1 d2) = j n m (sem f r90 es r45 a j en d1) (sem f r90 es r45 a j en d2)
sem f r90 es r45 a j en (Encimar d1 d2) = en (sem f r90 es r45 a j en d1) (sem f r90 es r45 a j en d2)

--------------------------- Mas Funciones ----------------------------
type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
limpia :: Pred a -> a -> Dibujo a -> Dibujo a
limpia p a d = cambia (limpiaAux p a) d

limpiaAux :: Pred a -> a -> a -> Dibujo a
limpiaAux p a b | p b = pureDib a black
                | otherwise = pureDib b black

-- Alguna básica satisface el predicado
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p d = sem p booleanId booleanId booleanId booleanNumOr booleanNumOr booleanOr d

-- Aux de anyDib
booleanId :: Bool -> Bool
booleanId b = b

booleanOr :: Bool -> Bool -> Bool
booleanOr b1 b2 = b1 || b2

booleanNumOr :: Int -> Int -> Bool -> Bool -> Bool
booleanNumOr _ _ b1 b2 = booleanOr b1 b2

-- Todas las básicas satisfacen el predicado
allDib :: Pred a -> Dibujo a -> Bool
allDib p d = sem p booleanId booleanId booleanId booleanNumAnd booleanNumAnd booleanAnd d

-- Aux de allDib
booleanAnd :: Bool -> Bool -> Bool
booleanAnd b1 b2 = b1 && b2

booleanNumAnd :: Int -> Int -> Bool -> Bool -> Bool
booleanNumAnd _ _ b1 b2 = booleanAnd b1 b2

-- Describe la figura. Ejemplos:
--   desc (Basica b) (const "b") = "b"
--   desc (Rotar fa) db = "rot (" ++ desc fa db ++ ")"
-- la descripción de cada constructor son sus tres primeros
-- símbolos en minúscula.
desc :: (a -> String) -> Dibujo a -> String
desc f d = sem f descRot descEsp descRot descApi descJun descEnc d

-- Aux de desc
descBasica :: Show a => a -> String
descBasica d = show d

descRot :: String -> String
descRot s = ("rot " ++ "(" ++ s ++ ")")

descEsp :: String -> String
descEsp s = ("esp " ++ "(" ++ s ++ ")")

descApi :: Int -> Int -> String -> String -> String
descApi i1 i2 s1 s2 = ("api " ++ show i1 ++ " "++ show i2 ++ " (" ++ s1 ++ ") " ++ "(" ++ s2 ++ ")")

descJun :: Int -> Int -> String -> String -> String
descJun i1 i2 s1 s2 = ("jun " ++ show i1 ++ " "++ show i2 ++ " (" ++ s1 ++ ") " ++ "(" ++ s2 ++ ")")

descEnc :: String -> String -> String
descEnc s1 s2 = ("enc " ++ "(" ++ s1 ++ ") " ++ "(" ++ s2 ++ ")")

-- Junta todas las figuras básicas de un dibujo
every :: Dibujo a -> [a]
every d = sem everyBas everyId everyId everyId everyConcatNum everyConcatNum everyConcat d

-- Aux de every
everyBas :: a -> [a]
everyBas d = [d]

everyId :: [a] -> [a]
everyId as = as

everyConcat :: [a] -> [a] -> [a]
everyConcat as1 as2 = as1 ++ as2

everyConcatNum :: Int -> Int -> [a] -> [a] -> [a]
everyConcatNum _ _ as1 as2 = everyConcat as1 as2

-- Cuenta la cantidad de veces que aparecen las básicas en una
-- figura.
contar :: Eq a => Dibujo a -> [(a, Int)]
contar d = sem contarBas contarId contarId contarId contarConcatNum contarConcatNum contarConcat d

-- Aux de contar
contarBas :: a -> [(a, Int)]
contarBas a = [(a,1)]

contarId :: [(a, Int)] -> [(a, Int)]
contarId as = as

contarConcatNum :: Eq a => Int -> Int -> [(a, Int)] -> [(a, Int)] -> [(a, Int)]
contarConcatNum _ _ as1 as2 = contarConcat as1 as2

contarConcat :: Eq a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
contarConcat [] ys = ys
contarConcat ((a, b):xs) ys = contarConcat xs (agregarConcat (a, b) ys)

-- Revisa si la tupla esta en la lista, si no esta la pega al final, si
-- esta suma la cantdad de veces que aparecio
agregarConcat :: Eq a => (a, Int) -> [(a, Int)] -> [(a, Int)]
agregarConcat (a,b) [] = [(a,b)]
agregarConcat (a,b) ((c,d):ys) | (a == c) = [(c, b+d)] ++ ys
                               | otherwise = [(c,d)] ++ (agregarConcat (a, b) ys)

------------------------ Predicados sobre Figuras -----------------------
-- Hay 4 rotaciones seguidas (empezando en el tope)
esRot360 :: Pred (Dibujo a)
esRot360 (Rotar90 (Rotar90 (Rotar90 (Rotar90 a)))) = True
esRot360 Vacio = False
esRot360 (Basica a _ ) = False
esRot360 (Rotar90 a) = esRot360 a
esRot360 (Espejar a) = esRot360 a
esRot360 (Rotar45 a) = esRot360 a
esRot360 (Apilar n m a b) = (esRot360 a) || (esRot360 b)
esRot360 (Juntar n m a b) = (esRot360 a) || (esRot360 b)
esRot360 (Encimar a b) = (esRot360 a) || (esRot360 b)

-- Hay 2 espejados seguidos (empezando en el tope)
esFlip2 :: Pred (Dibujo a)
esFlip2 (Espejar (Espejar a)) = True
esFlip2 Vacio = False
esFlip2 (Basica a _ ) = False
esFlip2 (Rotar90 a) = esFlip2 a
esFlip2 (Espejar a) = esFlip2 a
esFlip2 (Rotar45 a) = esFlip2 a
esFlip2 (Apilar n m a b) = (esFlip2 a) || (esFlip2 b)
esFlip2 (Juntar n m a b) = (esFlip2 a) || (esFlip2 b)
esFlip2 (Encimar a b) = (esFlip2 a) || (esFlip2 b)



isLeft :: Either a b -> Bool
isLeft (Left _ ) = True
isLeft (Right _ ) = False

------------------------- Funciones Debugeo -------------------------------
  -- la cadena que se toma como parámetro es la descripción
  -- del error.
check :: Pred (Dibujo a) -> String -> Dibujo a -> Either String (Dibujo a)
check p s d | p d = Left s
            | otherwise = Right d

  -- aplica todos los chequeos y acumula todos los errores,
  -- sólo devuelve la figura si no hubo ningún error.
todoBien :: Dibujo a -> Either [String] (Dibujo a)
todoBien d | esFlip2 d && esRot360 d = Left ["Es Flip 2", "Es Rot 360"]
           | esFlip2 d = Left ["Es Flip 2"]
           | esRot360 d = Left ["Es Rot 360"]
           | otherwise = Right d

------------------------ Funciones Correctoras -------------------------
-- Si encuentro 4 rotaciones90 seguidas las saco.
noRot360 :: Dibujo a -> Dibujo a
noRot360 (Rotar90(Rotar90(Rotar90(Rotar90 a)))) = noRot360 a
noRot360 Vacio = Vacio
noRot360 (Basica a c) = pureDib a c
noRot360 (Rotar90 a) = r90 (noRot360 a)
noRot360 (Espejar a) = espejar (noRot360 a)
noRot360 (Rotar45 a) = r45 (noRot360 a)
noRot360 (Apilar n m a b) = apilar n m (noRot360 a) (noRot360 b)
noRot360 (Juntar n m a b) = juntar n m (noRot360 a) (noRot360 b)
noRot360 (Encimar a b) = encimar (noRot360 a) (noRot360 b)

-- Si encuentro 2 espejar seguidos los saco.
noFlip2  :: Dibujo a -> Dibujo a
noFlip2 (Espejar (Espejar a)) = noFlip2 a
noFlip2 Vacio = Vacio
noFlip2 (Basica a c) = pureDib a c
noFlip2 (Rotar90 a) = r90 (noFlip2 a)
noFlip2 (Espejar a) = espejar(noFlip2 a)
noFlip2 (Rotar45 a) = r45(noFlip2 a)
noFlip2 (Apilar n m a b) = apilar n m (noFlip2 a) (noFlip2 b)
noFlip2 (Juntar n m a b) = juntar n m (noFlip2 a) (noFlip2 b)
noFlip2 (Encimar a b) = encimar (noFlip2 a) (noFlip2 b)

-- deben satisfacer
-- (1) check esRot360 "foo" (noRot360 f) = Right f', para alguna f'
-- (2) check esFlip2 "foo" (noFlip2 f) = Right f', para alguna f'

{- Tests:
    noFlip2(noRot360 (Rotar90(Rotar90(Rotar90(Rotar90(Espejar(Espejar(Rotar90(Rotar90(Rotar90(Rotar90(Basica 9))))))))))))
    noRot360 (Rotar90(Rotar90(Rotar90(Rotar90(Espejar(Espejar(Rotar90(Rotar90(Rotar90(Rotar90(Basica 9)))))))))))
    noFlip2 (Rotar90(Rotar90(Rotar90(Rotar90(Espejar(Espejar(Rotar90(Rotar90(Rotar90(Rotar90(Basica 9)))))))))))
    esRot360(noRot360 (Rotar90(Rotar90(Rotar90(Rotar90(Espejar(Espejar(Rotar90(Rotar90(Rotar90(Rotar90(Basica 9))))))))))))
-}
