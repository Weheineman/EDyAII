module ListSeq where

import Seq
import Par


-- ~ Genera la lista al reves (pues agregar al principio es mas eficiente)
auxTabulateS :: (Int -> a) -> Int -> [a]
auxTabulateS f 0 = [f 0]
auxTabulateS f n = x:xs
  where (x,xs) = f n ||| auxTabulateS f (n-1)
    
contractL :: (a -> a -> a) -> [a] -> [a]
contractL f []         = []
contractL f [x]        = [x]
contractL f (x1:x2:xs) =
    let (y,ys) = f x1 x2 ||| contractL f xs
    in  y:ys

expandL :: (a -> a -> a) -> [a] -> [a] -> [a]
expandL f [] _             = []
expandL f [x] (y:ys)        = [y]
expandL f (x1:x2:xs) (y:ys) =
    let (z, zs) = f y x1 ||| expandL f xs ys
    in  y:z:zs

instance Seq [] where
    emptyS = []
    
    singletonS x = [x]
    
    lengthS = length
    
    nthS (x:xs) n
        | n==0      = x
        | otherwise = nthS xs (n-1)
    
    -- ~ Confio en que reverse es O(n)
    tabulateS f n    = reverse $ auxTabulateS f (n-1)
    
    mapS f []        = []
    mapS f (x:xs)    = y:ys
        where (y,ys) = f x ||| mapS f xs
    
    filterS f [] = []
    filterS f (x:xs)
        | valid     = x:ys
        | otherwise = ys
      where (valid, ys) = f x ||| filterS f xs
    
    appendS = (++)
    
    takeS xs n = take n xs
    
    dropS xs n = drop n xs
    
    -- ~ Paralelizar no mejora la complejidad, pero confio en que es mas rapido
    showtS []   = EMPTY
    showtS [x]  = ELT x
    showtS xs   =
        let len    = quot (lengthS xs) 2
            (l, r) = takeS xs len ||| dropS xs len
        in  NODE l r
    
    showlS []       = NIL
    showlS (x:xs)   = CONS x xs
        
    joinS = concat
    
    reduceS f b []  = b
    reduceS f b [x] = f b x
    reduceS f b xs  = reduceS f b $ contractL f xs
    
    scanS f b []  = ([], b)
    scanS f b [x] = ([b], f b x)
    scanS f b xs  =
        let cl        = contractL f xs
            (pl, res) = scanS f b cl
        in  (expandL f xs pl, res)
    
    fromList = id
    
