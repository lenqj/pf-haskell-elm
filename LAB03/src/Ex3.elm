module Ex3 exposing (..)
import Maybe
safeDiv : Int -> Int -> Maybe Int
safeDiv a b = if ( b == 0 ) then Nothing
    else Just (a//b)
last: List a -> Maybe a
last a = 
    let
        lasth lst acc = 
            case lst of
                [] -> acc
                x::xs -> lasth xs (Just x)
    in 
        lasth a Nothing

len: List a -> Int
len a =
    case a of
        [] -> 0
        x::xs -> 1 + len(xs)

