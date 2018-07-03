import ArrSeq
import Seq

-- ~ Ejercicio 1

segundo :: (a,a,a,a) -> a
segundo (_, x, _, _) = x

mult :: Num a => (a,a,a,a) -> (a,a,a,a) -> (a,a,a,a)
mult (a1, b1, c1, d1) (a2, b2, c2, d2) =
    (a1*a2 + b1*c2, a1*b2 + b1*d2, c1*a2 + d1*c2, c1*b2 + d1*d2)

ident = (1,0,0,1)

matrGen  :: Integral a => Int -> (a,a,a,a)
matrGen _ = (1,1,1,0)

fibSeq :: Seq s => Int -> s Int
fibSeq n = mapS segundo s
  where (s, ans) = scanS mult ident $ tabulateS matrGen n

-- ~ Ejercicio 2
reverseS :: Seq s => s a -> s a
reverseS s = tabulateS (\x -> nthS s $ len - x - 1) len
  where len = lengthS s

-- ~ My code doesn't work and I don't know why
-- ~ aguaHist :: (Seq s, Num a, Ord a) => s a -> a
-- ~ aguaHist s =
    -- ~ let maxL = fst $ scanS max 0 s
        -- ~ maxR = reverseS . fst . scanS max 0 $ reverseS s
        -- ~ agua i = max 0 $ min (nthS maxL i) (nthS maxR i) - (nthS s i)
    -- ~ in  reduceS (+) 0 $ tabulateS agua (lengthS s)


-- ~ Ejercicio 3
data Paren = Open | Close

charToParen :: Char -> Paren
charToParen '(' = Open
charToParen ')' = Close

parenToInt :: Paren -> Int
parenToInt Open = 1
parenToInt Close = -1


matchParen :: (Seq s) =>  s Paren -> Bool
matchParen s = 
    let (seq, ans) = scanS (+) 0 $ mapS parenToInt s
    in  ans==0 && (reduceS (&&) True $ mapS (\x-> x>=0) seq)


-- ~ Ejercicio 4
data Point = (Int, Int)

euclidDist :: RealFloat a => Point -> Point -> a
euclidDist (x1, y1) (x2, y2) = sqrt . fromIntegral $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)

closestPair :: (Seq s, RealFloat a) => s Point -> a
