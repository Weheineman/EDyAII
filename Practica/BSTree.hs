data Bin a = Hoja | Nodo (Bin a) a (Bin a)

minBST:: Bin a -> a
minBST (Nodo Hoja a r) = a
minBST (Nodo l a r) = minBST l

maxBST:: Bin a -> a
maxBST (Nodo l a Hoja) = a
maxBST (Nodo l a r) = maxBST r

--Mas inutil que una clase de teoria de estructuras para Faltori
isHoja:: Bin a -> Bool
isHoja Hoja = True
isHoja _ = False

--If it's stupid but it works, it ain't stupid :)
checkBST:: Ord a => Bin a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja a Hoja) = True
checkBST (Nodo l a Hoja) = a >= (maxBST l) && (checkBST l)
checkBST (Nodo Hoja a r) = a <= (minBST r) && (checkBST r)
checkBST (Nodo l a r) = (a <= (minBST r)) && (checkBST r) && (a >= (maxBST l)) && (checkBST l)
