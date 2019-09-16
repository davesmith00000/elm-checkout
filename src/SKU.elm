module SKU exposing (SKU(..), asString, fromString)


type SKU
    = A
    | B
    | C
    | D


fromString : String -> Maybe SKU
fromString str =
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


asString : SKU -> String
asString sku =
    case sku of
        A ->
            "a"

        B ->
            "b"

        C ->
            "c"

        D ->
            "d"
