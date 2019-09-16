module NonEmptyList exposing (..)

{-
   There is a package that does this, but I felt like
   seeing if I could put it together.
-}


type alias NonEmptyList a =
    ( a, List a )


pure : a -> NonEmptyList a
pure value =
    ( value, [] )


fromTuple : ( a, List a ) -> NonEmptyList a
fromTuple t =
    t


fromList : List a -> Maybe (NonEmptyList a)
fromList l =
    case l of
        [] ->
            Nothing

        x :: xs ->
            Just (fromTuple ( x, xs ))



{-
   No infix operators! Gah!
   Intended to implement: `(::) : a -> NonEmptyList a -> NonEmptyList a`
-}


prepend : a -> NonEmptyList a -> NonEmptyList a
prepend value nel =
    ( value, toList nel )


toList : NonEmptyList a -> List a
toList nel =
    head nel :: tail nel


head : NonEmptyList a -> a
head nel =
    Tuple.first nel


tail : NonEmptyList a -> List a
tail nel =
    Tuple.second nel


length : NonEmptyList a -> Int
length nel =
    List.length (tail nel) + 1
