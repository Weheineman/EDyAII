{-
--Ejercicio 1
tad lista (A:Set) where
	import bool
	nil : lista A
	cons : a -> lista A -> lista A
	null : lista A -> Bool
	head : lista A -> A
	tail : lista A -> lista A

Especificacion algebraica
null nil = True
null (cons x xs) = false
head (cons x xs) = x
tail (cons x xs) = xs


Especificacion con modelo de secuencias
nil = <>
cons x <x1, ..., xn> = <x, x1, ..., xn>
null <x1, ..., xn> = True si n = 0
null <x1, ..., xn> = False en otro caso
head <x1, ..., xn> = x1
tail <x1, ..., xn> = <x2, ..., xn>

inL : List A -> A -> Bool
inL x <x1, .., xn> = True si existe i en [1,n] tal que x == xi
inL x <x1, .., xn> = False en otro caso

del : List A -> A -> List A
del nil x = nil

-}
