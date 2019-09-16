module Main exposing (..)

{-
   PLEASE NOTE: I wrote this interface bit very, very quickly indeed...
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
