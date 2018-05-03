{- 1)
    a)test::(Num a,Eq a) => (a -> a)->a->Bool
    b)esMenor::Ord a => a->a->Bool
    c)eq::Eq a => a->a->Bool
    showVal::Show a => a->[Char]
    2)
     a)(+5)::Num a => a->a
     b)(0<)::Ord a => a->a
     c)('a':)::[Char]->[Char]
     d)(++"\n")::[Char]->[Char]
     e)filter(==7)::Eq a =>[a] ->[a]
     f)map(++1)::Num a => [a]->[a]
    3)
     a)f g = g (1::int)::Int
     b)+
     c)f g = g
    4)
     a)False
     b)no anda error sintactico
     c)False
     d)no anda error de tipos
     e)0
     f)True
     g)True
    5)
     a) f x = x
     b) greater(x,y) = x > y
     c) f (x,y) = x
    6)
     a)smallest(x,y,z)| x '<= y `and` x `<=` z   = x
                      | y `<=` z `and` y `<=` x   = y
                      | otherwise           = z
                      
     b)second = \x -> (\y -> y)
     c)andThen = \b -> (\y -> if b then y else False)
     d)twice = \f -> (\x -> f (f x))
     e)flip = \f -> (\x -> (\y -> f y x))
     f)inc = \x -> x+1 --Queda de tipo Int :(
    7)
	 a)iff True y  = not y
	   iff False y = y
	 b)alpha x = x
	8)
	 h :: a -> b -> d
	 La primera, son equivalentes.
     
-}

alpha x = x

{- Por que no anda?
smallest = \x -> (\y -> (\z -> if (x<=y && x<=z)
							   then x 
							   else if (y<=z && y<=x) 
							        then y 
							        else z))
-}
