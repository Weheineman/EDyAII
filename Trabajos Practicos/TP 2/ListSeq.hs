{- Implementación del TAD secuencia -}
import Seq
import Par

instance Seq [] where
    emptyS = []
    singletonS x = [x]
    lengthS xs = length xs
    nthS (x:xs) n
        | n==0         = x
        | otherwise = nthS xs (n-1)
    {-Hay que paralelizar-}
    tabulateS f n = []
    mapS = map
    filterS = filter
    takeS xs n = take n xs
    dropS xs n = drop n xs
    fromList xs = xs
    
