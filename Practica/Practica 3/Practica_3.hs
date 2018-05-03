--Ejercicio 1
data Color = C{rojo::Float
              ,verde::Float
              ,azul::Float
              } deriving Show
              
mezclar :: Color -> Color -> Color
mezclar x y = C{rojo = rojo(x) + rojo(y)
               ,verde = verde(x) + verde(y)
               ,azul = azul(x) + azul(y)
               }

--Ejercicio 2
data Linea = L{len::Int
              ,cadena::[Char]
              ,pos:: Int
              } deriving Show

vacia:: Linea
vacia = L{len = 0
         ,cadena = []
         ,pos = 0
         }


moverIzq:: Linea -> Linea
moverIzq (L l cad p) = L l cad (max (p-1)  0)

moverDer:: Linea -> Linea
moverDer (L l cad p) = L l cad (min (p+1)  l)

moverIni:: Linea -> Linea
moverIni (L l cad p) = L l cad 0

moverFin:: Linea -> Linea
moverFin (L l cad p) = L l cad l

insertar:: Char -> Linea -> Linea
insertar c (L l cad p) = L (l+1) ((take p cad) ++ [c] ++ (drop p cad)) (p+1)

borrar:: Linea -> Linea
borrar (L l cad 0) = L l cad 0
borrar (L l cad p) = L (l-1) ((take (p-1) cad) ++ (drop p cad)) (p-1)

--Ejercicio 3

data CList a = EmptyCL | CLUnit a | Consnoc a (CList a) a deriving Show

isEmptyCl:: CList a -> Bool
isEmptyCl EmptyCL = True
isEmptyCl _ = False

isCLUnit:: CList a -> Bool
isCLUnit  (CLUnit _ ) = True
isCLUnit _ = False

headCL:: CList a -> a
headCL (CLUnit x ) = x
headCL (Consnoc x _ _ ) = x

tailCL:: CList a -> CList a
tailCL EmptyCL = EmptyCL
tailCL (CLUnit _ ) = EmptyCL
tailCL (Consnoc _ EmptyCL y) = CLUnit y
tailCL (Consnoc _ x y) = Consnoc (headCL x) (tailCL x) y

reverseCL:: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CLUnit x) = CLUnit x
reverseCL (Consnoc x y z) = Consnoc z (reverseCL y) x

cons:: a -> CList a -> CList a
cons x EmptyCL = CLUnit x
cons x (CLUnit y) = (Consnoc x EmptyCL y)
cons x (Consnoc w y z) = Consnoc x (cons w y) z  

snoc:: CList a -> a -> CList a
snoc EmptyCL x = CLUnit x
snoc (CLUnit y) x = Consnoc y EmptyCL x
snoc (Consnoc a b c) x = Consnoc a (snoc b c) x

inits:: CList a -> [CList a]
inits EmptyCL = [EmptyCL]
inits (CLUnit x) = [EmptyCL, CLUnit x]
inits (Consnoc x y z) = (inits (snoc y z))++[(Consnoc x y z)]

concatdosCL:: CList a -> CList a -> CList a
concatdosCL x EmptyCL = x
concatdosCL EmptyCL y = y
concatdosCL x (CLUnit y) = Consnoc (headCL x) (tailCL x) y
concatdosCL x y = concatdosCL (concatdosCL x (CLUnit (headCL y))) (tailCL y)
