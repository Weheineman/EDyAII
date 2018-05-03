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
-}

($$)::Bool->Bool->Bool
True $$ True = True
a $$ b 	= False

f x x = True
f x _ = False
