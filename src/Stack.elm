{-
   LIFO
-}


module
    Stack
    ( Stack
    , fromList
    , toList
    , push
    , pop
    , drop
    , dropN
    )
    where


type alias Stack a =
    List a


fromList : List a -> Stack a
fromList =
    List.reverse


toList : Stack a -> List a
toList =
    List.reverse


push : a -> Stack a -> Stack a
push x stack =
    x :: stack


pop : Stack a -> ( Maybe a, Stack a )
pop stack =
    case stack of
        [] ->
            ( Nothing, stack )

        x :: xs ->
            ( Just x, xs )



{- drop last element -}


drop : Stack a -> Stack a
drop stack =
    case stack of
        [] ->
            stack

        x :: xs ->
            xs



{- drop last N elements -}


dropN : Int -> Stack a -> Stack a
dropN n stack =
    if n <= 0 then
        stack
    else
        dropN (n - 1) (drop stack)
