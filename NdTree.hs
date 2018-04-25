data NdTree p = Node (NdTree p) -- sub´arbol izquierdo
                p               -- punto
                (NdTree p)      -- sub´arbol derecho
                Int             -- eje
                | Empty
  deriving (Eq, Ord, Show)

class Punto p where
    dimension :: p -> Int       -- devuelve el n´umero de coordenadas de un punto
    coord :: Int -> p -> Double -- devuelve la coordenada k-´esima de un punto (comenzando de 0)
    dist :: p -> p -> Double    -- calcula la distancia entre dos puntos


--Ejercicio 1
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
getAxis :: Punto p => p -> Int -> Int
getAxis p level = mod level $ dimension p

getPivot :: Punto p => p -> Int -> Double
getPivot p level = coord (getAxis p level) p

singularNd :: Punto p => p -> Int -> NdTree p
singularNd p level = Node Empty p Empty $ getAxis p level

checkLT :: Punto p => p -> Int -> p -> Bool
checkLT pivot_pt level p = (getPivot p level) < (getPivot pivot_pt level)

checkGT :: Punto p => p -> Int -> p -> Bool
checkGT pivot_pt level p = (getPivot p level) > (getPivot pivot_pt level)

fromListAux :: Punto p => [p] -> Int -> NdTree p
fromListAux [] _ = Empty
fromListAux [x] level = singularNd x level
fromListAux (x:xs) level = Node (fromListAux lt_list $ level+1) x (fromListAux gt_list $ level+1) axis
  where
    lt_list = filter (checkLT x level) xs
    gt_list = filter (checkGT x level) xs
    axis = getAxis x level

fromList :: Punto p => [p] -> NdTree p
fromList plist = fromListAux plist 0
