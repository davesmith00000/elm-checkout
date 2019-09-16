module Checkout exposing (..)

import Debug exposing (log)
import Dict exposing (Dict)
import NonEmptyList exposing (..)
import SKU exposing (SKU(..))


type alias PriceInfo =
    { sku : SKU
    , price : Int
    , discount : Maybe BulkDiscount
    }


type alias BulkDiscount =
    { multiplier : Int
    , price : Int
    }


priceMap : Dict String PriceInfo
priceMap =
    Dict.fromList
        [ ( SKU.asString A, PriceInfo A 50 (Just <| BulkDiscount 3 130) )
        , ( SKU.asString B, PriceInfo B 30 (Just <| BulkDiscount 2 45) )
        , ( SKU.asString C, PriceInfo C 20 Nothing )
        , ( SKU.asString D, PriceInfo D 10 Nothing )
        ]


calculateTotal : List SKU -> Int
calculateTotal skus =
    let
        grouped =
            groupBy skus Dict.empty

        keys =
            Dict.keys grouped

        f : String -> Maybe Int
        f key =
            Maybe.map2 calculateTotalPerSKU (Dict.get key grouped) (Dict.get key priceMap)

        combine : Maybe Int -> Int -> Int
        combine maybeInt acc =
            acc + Maybe.withDefault 0 maybeInt
    in
    List.foldl combine 0 (List.map f keys)


calculateTotalPerSKU : NonEmptyList SKU -> PriceInfo -> Int
calculateTotalPerSKU nel priceInfo =
    let
        quantity =
            NonEmptyList.length nel

        normalPrice =
            priceInfo.price
    in
    Maybe.map (\discountInfo -> discountTotalCalculation quantity normalPrice discountInfo) priceInfo.discount
        |> Maybe.withDefault (normalTotalCalculation quantity normalPrice)


discountTotalCalculation : Int -> Int -> BulkDiscount -> Int
discountTotalCalculation quantity price discountInfo =
    normalTotalCalculation (quantity // discountInfo.multiplier) discountInfo.price
        -- discount
        + normalTotalCalculation (remainderBy discountInfo.multiplier quantity) price



-- left over normal


normalTotalCalculation : Int -> Int -> Int
normalTotalCalculation quantity price =
    quantity * price


groupBy : List SKU -> Dict String (NonEmptyList SKU) -> Dict String (NonEmptyList SKU)
groupBy skus acc =
    case skus of
        [] ->
            acc

        s :: ss ->
            groupBy ss (updateDict (SKU.asString s) s acc)


updateDict : String -> SKU -> Dict String (NonEmptyList SKU) -> Dict String (NonEmptyList SKU)
updateDict key value store =
    Dict.get key store
        |> Maybe.map (\l -> Dict.insert key (NonEmptyList.prepend value l) store)
        |> Maybe.withDefault (Dict.insert key (NonEmptyList.pure value) store)
