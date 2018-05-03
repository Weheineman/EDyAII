--Ejercicio 1
data Color = C{rojo::Float
              ,verde::Float
              ,azul::Float
              } deriving Show
              
mezclar :: Color -> Color -> Color
mezclar x y = C (rojo x + rojo y) (verde x + verde y) (azul x + azul y)

--Ejercicio 2
data Linea = L{len::Int
              ,cadena::[Char]
              ,pos:: Int
              } deriving Show

vacia:: Linea
vacia = L 0 [] 0

moverIzq :: Linea -> Linea
moverIzq (L l cad p) = L l cad (max (p-1)  0)

moverDer :: Linea -> Linea
moverDer (L l cad p) = L l cad (min (p+1)  l)

moverIni :: Linea -> Linea
moverIni (L l cad p) = L l cad 0

moverFin :: Linea -> Linea
moverFin (L l cad p) = L l cad l

insertar :: Char -> Linea -> Linea
insertar c (L l cad p) = L (l+1) ((take p cad) ++ [c] ++ (drop p cad)) (p+1)

borrar :: Linea -> Linea
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

headCL :: CList a -> a
headCL (CLUnit x ) = x
headCL (Consnoc x _ _ ) = x

tailCL :: CList a -> CList a
tailCL EmptyCL = EmptyCL
tailCL (CLUnit _ ) = EmptyCL
tailCL (Consnoc _ EmptyCL y) = CLUnit y
tailCL (Consnoc _ x y) = Consnoc (headCL x) (tailCL x) y

reverseCL :: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CLUnit x) = CLUnit x
reverseCL (Consnoc x y z) = Consnoc z (reverseCL y) x

--Agrega al principio
cons :: a -> CList a -> CList a
cons x EmptyCL = CLUnit x
cons x (CLUnit y) = (Consnoc x EmptyCL y)
cons x (Consnoc w y z) = Consnoc x (cons w y) z  

--Agrega al final
snoc :: a -> CList a -> CList a
snoc x EmptyCL = CLUnit x
snoc x (CLUnit y) = Consnoc y EmptyCL x
snoc x (Consnoc a b c) = Consnoc a (snoc c b) x

lasts :: CList a -> [CList a]
lasts EmptyCL = [EmptyCL]
lasts (CLUnit x) = [EmptyCL, CLUnit x]
lasts (Consnoc x y z) = (lasts (snoc z y))++[(Consnoc x y z)]

inits :: CList a -> [CList a]
inits x = map reverseCL (lasts $ reverseCL x)

concatdosCL :: CList a -> CList a -> CList a
concatdosCL x EmptyCL = x
concatdosCL EmptyCL y = y
concatdosCL x y = cons (headCL x) $ concatdosCL (tailCL x) y

concatCL :: CList (CList a) -> CList a
concatCL EmptyCL = EmptyCL
concatCL (CLUnit x) = x
concatCL (Consnoc x y z) = concatdosCL (concatdosCL x $ concatCL y) z

--Ejercicio 4
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

eval :: Aexp -> Int
eval (Num x) = x
eval (Prod x y) = (eval x) * (eval y)
eval (Div x y)
	| divisor /= 0 = (eval x) `div` divisor
		where divisor = eval y

seval :: Aexp -> Maybe Int
seval (Num x) = Just x
seval (Prod x y) = case (seval x, seval y) of
	(Nothing, _) -> Nothing
	(_, Nothing) -> Nothing
	(Just a, Just b) -> Just $ a * b
seval (Div x y) = case (seval x, seval y) of
	(Nothing, _) -> Nothing
	(_, Nothing) -> Nothing
	(_, Just 0) -> Nothing
	(Just a, Just b) -> Just $ div a b
			
--Ejercicio 5
data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving Show

completo :: a -> Int -> Bin a
completo x 0 = Nodo Hoja x Hoja
completo x n
	| n > 0 = Nodo subarbol x subarbol
		where subarbol = completo x $ n-1
		
balanceado :: a -> Int -> Bin a
balanceado _ 0 = Hoja

-- ~ Falta hacer balanceado 

--Ejercicio 6
data GenTree a = EmptyG | NodeG a [GenTree a] deriving Show
data BinTree a = EmptyB | NodeB (BinTree a) a (BinTree a) deriving Show

-- ~ g2bt :: Gentree a -> BinTree a
-- ~ g2bt EmptyG = EmptyB
-- ~ g2bt (NodeG a []) = NodeB EmptyB a EmptyB
-- ~ g2bt (NodeG a (x:xs)) = NodeB x a 
