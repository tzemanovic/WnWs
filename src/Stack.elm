{-
LIFO
-}
module Stack
    (Stack
    ,fromList
    ,toList
    ,push
    ,pop
   ) where


type alias Stack a = List a

fromList : List a -> Stack a
fromList = List.reverse

toList : Stack a -> List a
toList = List.reverse

push : a -> Stack a -> Stack a
push x stack = x :: stack

pop : Stack a -> (Maybe a,Stack a)
pop stack = case stack of
    [] -> (Nothing,stack)
    (x :: xs) -> (Just x, xs) 
