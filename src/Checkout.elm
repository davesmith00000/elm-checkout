module Checkout exposing (..)


type SKU
    = A
    | B
    | C
    | D


stringToSKU : String -> Maybe SKU
stringToSKU str =
    case String.toLower str of
        "a" ->
            Just A

        "b" ->
            Just B

        "c" ->
            Just C

        "d" ->
            Just D

        _ ->
            Nothing
