module Main exposing (..)

{-
   PLEASE NOTE: I wrote this interface bit very, very quickly indeed...
   Some notes below...
-}

import Browser
import Checkout exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SKU exposing (SKU)


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = Calculate String


type alias Model =
    { input : String
    , total : Int
    }


init : Model
init =
    Model "" 0


update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate str ->
            { model | total = Checkout.calculateTotal <| parseInput str }


parseInput : String -> List SKU
parseInput str =
    List.filter SKU.isValid (String.toList (String.toLower str))
        |> List.map SKU.fromChar
        |> sequence
        |> Maybe.withDefault []



{-
   There is no `sequence` in the standard library as far as I can tell.
   There is a list extras lib that does have this I think.
   I don't think I've done this very well, I was in a hurry, I'm
   resisting the urge to rewrite it...
-}


sequence : List (Maybe a) -> Maybe (List a)
sequence l =
    let
        toList : Maybe a -> List a
        toList fa =
            case fa of
                Just a ->
                    [ a ]

                Nothing ->
                    []

        flatten : List (List a) -> List a -> List a
        flatten remaining acc =
            case remaining of
                [] ->
                    acc

                [] :: xs ->
                    flatten xs acc

                (x :: _) :: xs ->
                    flatten xs (x :: acc)
    in
    Just <| flatten (List.map toList l) []


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ placeholder "Enter SKUs here...", onInput Calculate ] [ text "" ]
            ]
        , div []
            [ text ("Total: " ++ String.fromInt model.total)
            ]
        ]
