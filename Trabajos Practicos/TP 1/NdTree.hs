-- ~ Estructuras de Datos y Algoritmos II
-- ~ Trabajo Practico 1
-- ~ Gianni Weinand

import Prelude

data NdTree p = Node (NdTree p) p (NdTree p) Int
              | Empty
  deriving (Eq, Ord, Show)

class (Eq p) => Punto p where
    dimension :: p -> Int       -- devuelve el n´umero de coordenadas de un punto
    coord :: Int -> p -> Double -- devuelve la coordenada k-´esima de un punto (comenzando de 0)
    dist :: p -> p -> Double    -- calcula la distancia entre dos puntos


--Ejercicio 1
--Devuelve el cuadrado de un numero
sq :: Num a => a -> a
sq x = x*x

newtype Punto2d = P2d (Double, Double)
instance Punto Punto2d where
    dimension _ = 2
    coord 0 (P2d (x, _)) = x
    coord 1 (P2d (_, y)) = y
    dist (P2d (x1, y1)) (P2d (x2, y2)) = sqrt $ (sq $ x1 - x2) + (sq $ y1 - y2)

newtype Punto3d = P3d (Double, Double, Double)
instance Punto Punto3d where
    dimension _ = 3
    coord 0 (P3d (x,_,_)) = x
    coord 1 (P3d (_,y,_)) = y
    coord 2 (P3d (_,_,z)) = z
    dist (P3d (x1, y1, z1)) (P3d (x2, y2, z2)) = sqrt $ (sq $ x1 - x2) + (sq $ y1 - y2) + (sq $ z1 - z2)


--para simplificar debugging
instance Show Punto2d where
    show (P2d p) = show p

instance Show Punto3d where
    show (P3d p) = show p

--Ejercicio 2
--Devuelve el eje dada la profundidad y un punto
getAxis :: Punto p => Int -> p -> Int
getAxis level p = mod level $ dimension p

--Devuelve el valor en la coordenada del eje
getAxValue :: Punto p => Int -> p -> Double
getAxValue level p = coord (getAxis level p) p

--Devuelve True sii p1 es menor a p2 en la coordenada del eje
checkLT :: Punto p => Int -> p -> p -> Bool
checkLT level p1 p2 = (getAxValue level p1) < (getAxValue level p2)

--Devuelve True sii p1 es mayor a p2 en la coordenada del eje
checkGT :: Punto p => Int -> p -> p -> Bool
checkGT level p1 p2 = (getAxValue level p1) > (getAxValue level p2)

--Quicksort que ordena segun la coordenada del eje
sortAxis :: Punto p => Int -> [p] -> [p]
sortAxis level [] = []
sortAxis level (x:xs) = (sortAxis level ltList) ++ [x] ++ (sortAxis level gtList)
    where ltList = filter (checkGT level x) xs -- Si el pivot es mayor
          gtList = filter (checkLT level x) xs -- Si el pivot es menor

--Devuelve el elemento que se encuentra en la mitad de la lista
getMiddle :: Punto p => [p] -> p
getMiddle l = l !! (div (length l) 2)

fromListAux :: Punto p => Int -> [p] -> NdTree p
fromListAux _ [] = Empty
fromListAux level l = 
    let axis = getAxis level $ head l -- El eje en este paso
        sorted = sortAxis level l -- La lista ordenada
        median = getMiddle sorted -- La mediana
        ltList = takeWhile (checkGT level median) sorted -- Lista de los menores a la mediana
        gtList = dropWhile (not . checkLT level median) sorted -- Lista de los mayores a la mediana
        ltTree = fromListAux (level+1) ltList -- Arbol de los menores
        gtTree = fromListAux (level+1) gtList -- Arbol de los mayores
    in  Node ltTree median gtTree axis

fromList :: Punto p => [p] -> NdTree p
fromList plist = fromListAux 0 plist

--Ejercicio 3
--Dos puntos son iguales sii son iguales todas sus coordenadas
instance Eq Punto2d where
    p1 == p2 = and [(coord i p1) == (coord i p2) | i <- [0..(dimension p1 - 1)]]

--Dos puntos son iguales sii son iguales todas sus coordenadas
instance Eq Punto3d where
    p1 == p2 = and [(coord i p1) == (coord i p2) | i <- [0..(dimension p1 - 1)]]

insertarAux :: Punto p => Int -> p -> NdTree p -> NdTree p
insertarAux level p Empty  = Node Empty p Empty $ getAxis level p --Si llegamos al final insertamos una hoja
insertarAux level p tree@(Node l d r ax)
    | p == d            = tree --Si ya existe el elemento no hacemos nada
    | checkLT level p d = Node (insertarAux (level+1) p l) d r ax --Si el elemento es menor se inserta en el subarbol izquierdo
    | otherwise         = Node  l d (insertarAux (level+1) p r) ax --Si no se inserta en el subarbol derecho

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p tree = insertarAux 0 p tree

--Ejercicio 4
--Devuelve el punto con menor valor en la coordenada dada
minPt :: Punto p => Int -> p -> p -> p
minPt ax p1 p2
    | checkLT ax p1 p2 = p1
    | otherwise        = p2

buscarMin :: Punto p => Int -> NdTree p -> p
buscarMin ax (Node Empty d Empty _) = d --Si es una hoja, el valor actual es el minimo
buscarMin ax (Node Empty d r _) = minPt ax d $ buscarMin ax r --Se calcula el minimo entre el valor actual y el del subarbol derecho
buscarMin ax (Node l d Empty _) = minPt ax d $ buscarMin ax l --Se calcula el minimo entre el valor actual y el del subarbol izquierdo
buscarMin ax (Node l d r ndAx) 
    | ndAx == ax = minPt ax d $ buscarMin ax l --Si los ejes son iguales se busca solo en el subarbol izquierdo
    | otherwise  = minPt ax d $ minPt ax (buscarMin ax l) (buscarMin ax r) --Si no se busca en ambos


--Devuelve el punto con mayor valor en la coordenada dada
maxPt :: Punto p => Int -> p -> p -> p
maxPt ax p1 p2
    | checkGT ax p1 p2 = p1
    | otherwise        = p2

buscarMax :: Punto p => Int -> NdTree p -> p
buscarMax ax (Node Empty d Empty _) = d --Si es una hoja, el valor actual es el minimo
buscarMax ax (Node Empty d r _) = maxPt ax d $ buscarMax ax r --Se calcula el minimo entre el valor actual y el del subarbol derecho
buscarMax ax (Node l d Empty _) = maxPt ax d $ buscarMax ax l --Se calcula el minimo entre el valor actual y el del subarbol izquierdo
buscarMax ax (Node l d r ndAx)
    | ndAx == ax = maxPt ax d $ buscarMax ax r --Si los ejes son iguales se busca solo en el subarbol derecho
    | otherwise  = maxPt ax d $ maxPt ax (buscarMax ax l) (buscarMax ax r) --Si no se busca en ambos


eliminar :: Punto p => p -> NdTree p -> NdTree p
eliminar p leaf@(Node Empty d Empty _) = --Si llegamos a una hoja y el valor actual es el buscado, se borra. Si no, se deja como esta.
    if p == d then Empty
              else leaf
eliminar p (Node l d Empty ax) = --Si no hay subarbol derecho y el valor actual es el buscado, se elimina (se reemplaza por uno del subarbol izq).
                                 --Si no, se sigue buscando en el subarbol izq.
    if p == d then (Node (eliminar maxL l) maxL Empty ax)
              else (Node (eliminar p l) d Empty ax)
                where maxL = buscarMax ax l
eliminar p (Node l d r ax) = --Si el valor actual es el buscado, se reemplaza por uno del subarbol der. Si no, se continua por el subarbol correspondiente.
    if p == d then (Node l minR (eliminar minR r) ax)
              else if (checkLT ax p d)
                       then (Node (eliminar p l) d r ax)
                       else (Node l d (eliminar p r) ax)
                         where minR = buscarMin ax r

--Ejercicio 5
type Rect = (Punto2d, Punto2d)

--Devuelve el menor double de los dados
minDoub :: Double -> Double -> Double
minDoub a b = if a < b then a else b

--Devuelve el mayor double de los dados
maxDoub :: Double -> Double -> Double
maxDoub a b = if a > b then a else b

-- Ordena las coordenadas, deja la esquina inf izq y la sup derecha.
ordRect :: Rect -> Rect
ordRect (P2d (x1, y1), P2d(x2, y2)) = ((P2d (minDoub x1 x2, minDoub y1 y2)), (P2d (maxDoub x1 x2, maxDoub y1 y2)))

--Devuelve true sii el punto esta dentro del rectangulo en la coordenada dada.
inCoordAux :: Int -> Punto2d -> Rect -> Bool
inCoordAux 0 (P2d (x, _)) (P2d (x1, _), P2d (x2, _)) = x1 <= x && x <= x2
inCoordAux 1 (P2d (_, y)) (P2d (_, y1), P2d (_, y2)) = y1 <= y && y <= y2

--Devuelve True sii el punto se encuentra dentro de los extremos del rectangulo
--en la coordenada dada
inCoord :: Int -> Punto2d -> Rect -> Bool
inCoord c p r = inCoordAux c p $ ordRect r

--Si el punto esta en el rectangulo, devuelve una lista con el punto.
--Sino devuelve lista vacia.
indCheck :: Punto2d -> Rect -> [Punto2d]
indCheck p r
    | inCoord 0 p r && (inCoord 1 p r) = [p]
    | otherwise                        = []

ortogonalSearchAux :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearchAux Empty _ = [] --Si el arbol esta vacio devuelve empty list.
ortogonalSearchAux tree@(Node l d r ax) rect@(p1, _)
    | inAx      = (indCheck d rect) ++ (ortogonalSearchAux l rect) ++ (ortogonalSearchAux r rect) --Si esta dentro se chequea la otra coordenada y se busca en ambos subarboles.
    | ltAx      = ortogonalSearchAux r rect --Si es menor busca en el subarbol derecho
    | otherwise = ortogonalSearchAux l rect --Si es mayor busca en el subarbol izquierdo
  where inAx = inCoord ax d rect
        ltAx = (coord ax d) < (coord ax p1)
        
ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch tree rect = ortogonalSearchAux tree $ ordRect rect
