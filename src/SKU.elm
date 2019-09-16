module SKU exposing (SKU(..), asString, fromChar, fromString, isValid)

{-
   I quite like the Elm idea of breaking into a new file based on data structure.
-}


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


fromChar : Char -> Maybe SKU
fromChar c =
    fromString <| String.fromChar c


isValid : Char -> Bool
isValid c =
    case fromChar c of
        Just _ ->
            True

        Nothing ->
            False


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
