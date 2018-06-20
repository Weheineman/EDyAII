module ArrSeq where

import Seq
import Par
import qualified Arr as A

-- ~ Solo llamo a contract cuando la secuencia tiene al menos 2 elementos
contractA :: (a -> a -> a) -> A.Arr a -> A.Arr a
contractA f s
    | even len  = tabulateS g half
    | otherwise = tabulateS h $ half+1
  where len  = lengthS s
        half = quot len 2
        g i  = f (nthS s $ 2*i) (nthS s $ 2*i + 1)
        h i  = if i == half then (nthS s $ 2*i) else g i

expandA :: (a -> a -> a) -> A.Arr a -> A.Arr a -> A.Arr a
expandA f s ps =
    let len = lengthS s
        g i = nthS ps $ quot i 2
        h i = if even i then g i else f (g i) (nthS s $ i-1)
    in  tabulateS h len

instance Seq A.Arr where
    emptyS = fromList []
    
    singletonS x = fromList [x]
    
    lengthS = A.length
    
    nthS = (A.!)
    
    tabulateS = A.tabulate
    
    mapS f s = tabulateS (f . nthS s) $ lengthS s
    
    filterS f s =
        let g x = if f x then singletonS x else emptyS
        in  joinS $ mapS g s
    
    appendS s ys = joinS $ fromList [s, ys]
    
    takeS s n = A.subArray 0 n s
    
    dropS s n = A.subArray n (lengthS s - n) s
    
    showtS s =
        case lengthS s of
            0   -> EMPTY
            1   -> ELT $ nthS s 0
            len ->
                let half = quot len 2
                in  NODE (takeS s half) $ dropS s half
    
    showlS s
        | lengthS s == 0 = NIL
        | otherwise       = CONS (nthS s 0) $ dropS s 1
    
    joinS = A.flatten
    
    reduceS f b s =
        case lengthS s of
            0 -> b
            1 -> f b $ nthS s 0
            _ -> reduceS f b $ contractA f s
    
    scanS f b s =
        case lengthS s of
            0 -> (emptyS, b)
            1 -> (singletonS b, f b $ nthS s 0)
            _ ->
                let cs = contractA f s
                    (ps, res) = scanS f b cs
                in (expandA f s ps, res)
    
    fromList = A.fromList
