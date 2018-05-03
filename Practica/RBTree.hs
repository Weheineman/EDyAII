data Color = R | B
data RBT a =  E | T Color (RBT a) a (RBT a)

memberRBT:: Ord a => a ->
