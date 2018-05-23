{-

--Ejercicio 1
tad Lista (Eq A => A:Set) where
	import Bool
	nil : Lista A
	cons : a -> Lista A -> Lista A
	null : Lista A -> Bool
	head : Lista A -> A
	tail : Lista A -> Lista A

Especificacion algebraica
null nil = True
null (cons x xs) = false
head (cons x xs) = x
tail (cons x xs) = xs


Especificacion con modelo de secuencias
nil = <>
cons x <x1, ..., xn> = <x, x1, ..., xn>
null <x1, ..., xn> = True 	si n = 0
null <x1, ..., xn> = False 	en otro caso
head <x1, ..., xn> = x1
tail <x1, ..., xn> = <x2, ..., xn>

inL : List A -> A -> Bool
inL x <x1, .., xn> = True 	si existe i en [1,n] tal que x == xi
inL x <x1, .., xn> = False 	en otro caso

del : List A -> A -> List A
del nil x = nil
del (cons y ys) x = del ys x   			si x=y
del (cons y ys) x = cons y (del ys x)  	en otro caso


--Ejercicio 2
tad Pila (A:Set) where
	empty : Pila A
	push : a -> Pila A -> Pila A
	isEmpty : Pila A -> Bool
	top : Pila A -> a
	pop : Pila A -> Pila A

Especificacion Algebraica
isEmpty empty = True
isEmpty (push x xs) = False
top (push x xs) = x
pop (push x xs) = xs

Especificacion con modelo de secuencias
empty = <>
push x <x1, ..., xn> = <x, x1, ..., xn>
isEmpty <x1, ..., xn> = True	si n=0
isEmpty <x1, ..., xn> = False	en otro caso
top <x1, ..., xn> = x1
pop <x1, x2, ..., xn> = <x2, ..., xn>


--Ejercicio 3
tad Conjunto (A : Set) where
	import Bool
	vacio : Conjunto A
	insertar : A -> Conjunto A -> Conjunto A
	borrar : A -> Conjunto A -> Conjunto A
	esVacı́o : Conjunto A -> Bool
	unión : Conjunto A -> Conjunto A -> Conjunto A
	intersección : Conjunto A -> Conjunto A -> Conjunto A
	resta : Conjunto A -> Conjunto A -> Conjunto A

Especificacion Algebraica
insertar x (insertar y c) = insertar x c				si x=y
insertar x (insertar y c) = insertar y (insertar x c)	si x!=y
borrar x vacio = vacio
borrar x (insertar y c) = c							si x=y
borrar x (insertar y c) = insertar y (borrar x c)	si x!=y
esVacio vacio = True
esVacio (insertar x c) = False
union (insertar x c) vacio = (insertar x c)
union vacio (insertar x c) = (insertar x c)
union (insertar x c1) (insertar y c2) = insertar x (insertar y (union c1 c2))
interseccion vacio c2 = vacio
interseccion c1 vacio = vacio
interseccion c1 c2 = vacio
interseccion (insertar x c1) (insertar y c2) = 
resta vacio c2 = vacio
resta c1 vacio = c1
resta c1 (insertar x c2) = resta (borrar x c1) c2

????????????????????????

--Ejercicio 4
tad PriorityQueue (A:Set) where
	vacia : PriorityQueue A
	poner : A -> 
	primero : PriorityQueue A -> A
	sacar : PriorityQueue A -> PriorityQueue A
	esVacia : PriorityQueue A -> Bool
	union : PriorityQueue A -> PriorityQueue A -> PriorityQueue A
	
Es
-}
