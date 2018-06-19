{- Implementación del TAD secuencia -}
module ListSeq where

import Seq
import Par


--Genera la lista al reves (pues agregar al principio es mas eficiente)
auxTabulateS :: (Int -> a) -> Int -> [a]
auxTabulateS f 0 = [f 0]
auxTabulateS f n = x:xs
    where (x,xs) = (f n) ||| (auxTabulateS f (n-1))

instance Seq [] where
    emptyS = []
    
    singletonS x = [x]
    
    lengthS xs = length xs
    
    nthS (x:xs) n
        | n==0      = x
        | otherwise = nthS xs (n-1)
    
    tabulateS f n    = reverse $ auxTabulateS f (n-1)
    
    mapS f []        = []
    mapS f (x:xs)    = y:ys
        where (y,ys) = (f x) ||| (mapS f xs)
    
    filterS f [] = []
    filterS f (x:xs)
        | check     = x:ys
        | otherwise = ys
      where (check, ys) = (f x) ||| (filterS f xs)
    
    appendS = (++)
    
    takeS xs n = take n xs
    
    dropS xs n = drop n xs
    
    showtS []   = EMPTY
    showtS [x]  = ELT x
    --No se como dividirlo pls askerino
    showtS xs   = NODE (takeS xs l) (dropS xs l)
      where l = quot (lengthS xs) 2
    
    showlS []       = NIL
    showlS (x:xs)   = CONS x xs
        
    fromList xs = xs
    
