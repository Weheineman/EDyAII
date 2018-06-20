module ArrSeq where

import Seq
import Par
import qualified Arr as A

instance Seq A.Arr where
    emptyS = fromList []
    
    singletonS x = fromList [x]
    
    lengthS = A.length
    
    nthS = (A.!)
    
    tabulateS = A.tabulate
    
    mapS f xs = tabulateS (f . nthS xs) $ lengthS xs
    
    -- ~ filterS = id
    
    appendS xs ys = joinS $ fromList [xs, ys]
    
    takeS xs n = A.subArray 0 n xs
    
    dropS xs n = A.subArray n (lengthS xs - n) xs
    
    showtS xs =
        case lengthS xs of
            0   -> EMPTY
            1   -> ELT $ nthS xs 0
            len ->
                let half = quot len 2
                in  NODE (takeS xs half) $ dropS xs half
    
    showlS xs
        | lengthS xs == 0 = NIL
        | otherwise       = CONS (nthS xs 0) $ dropS xs 1
    
    joinS = A.flatten
    
    -- ~ reduceS = id
    
    -- ~ scanS = id
    
    fromList = A.fromList
