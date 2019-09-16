module CheckoutTests exposing (..)

import Checkout exposing (..)
import Debug exposing (log)
import Dict exposing (Dict)
import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import NonEmptyList exposing (..)
import SKU exposing (SKU(..))
import Test exposing (..)


checkoutTests : Test
checkoutTests =
    describe "Checkout"
        [ describe "Checkout calculations"
            [ it "should be able to check the price of a single item" <|
                expect (calculateTotal [ A ]) to equal 50
            , it "should be able to check the price of a multiple items" <|
                expect (calculateTotal [ A, B, C, D ]) to equal (50 + 30 + 20 + 10)
            , it "should be able to check the price of a single discount" <|
                expect (calculateTotal [ A, A, A ]) to equal 130
            , it "should be able to check the price of a single discount + one extra" <|
                expect (calculateTotal [ A, A, A, A ]) to equal (130 + 50)
            , it "should be able to check the price of a multiple items with a discount" <|
                expect (calculateTotal [ A, A, A, A, B, C, D, B, B ]) to equal (130 + 50 + 30 + 20 + 10 + 45)
            ]
        , describe "Calculating total for an SKU" <|
            [ it "should be able to calulate the total for a single item" <|
                let
                    priceInfo =
                        PriceInfo A 50 Nothing

                    actual =
                        calculateTotalPerSKU (NonEmptyList.pure A) priceInfo

                    expected =
                        50
                in
                expect actual to equal expected
            , it "should be able to calulate the total for multiple items" <|
                let
                    priceInfo =
                        PriceInfo A 50 Nothing

                    actual =
                        calculateTotalPerSKU (NonEmptyList.fromTuple ( A, [ A, A ] )) priceInfo

                    expected =
                        150
                in
                expect actual to equal expected
            , it "should be able to calulate the total for multiple items that meet a discount" <|
                let
                    priceInfo =
                        PriceInfo A 50 (Just <| BulkDiscount 3 100)

                    actual =
                        calculateTotalPerSKU (NonEmptyList.fromTuple ( A, [ A, A ] )) priceInfo

                    expected =
                        100
                in
                expect actual to equal expected
            , it "should be able to calulate the total for multiple items that exceeds a discount" <|
                let
                    priceInfo =
                        PriceInfo A 50 (Just <| BulkDiscount 3 100)

                    actual =
                        calculateTotalPerSKU (NonEmptyList.fromTuple ( A, [ A, A, A ] )) priceInfo

                    expected =
                        150
                in
                expect actual to equal expected
            , it "should be able to calulate the total for multiple items that exceeds two discounts" <|
                let
                    priceInfo =
                        PriceInfo A 50 (Just <| BulkDiscount 3 100)

                    actual =
                        calculateTotalPerSKU (NonEmptyList.fromTuple ( A, [ A, A, A, A, A, A ] )) priceInfo

                    expected =
                        250
                in
                expect actual to equal expected
            ]
        , describe "Checkout discountTotalCalculation" <|
            [ it "should be able to calculate a discount" <|
                let
                    actual =
                        discountTotalCalculation 7 50 (BulkDiscount 3 100)

                    expected =
                        250
                in
                expect actual to equal expected
            ]
        , describe "Checkout updateDict" <|
            [ it "should be able to insert a new value" <|
                let
                    actual =
                        updateDict "a" A (Dict.fromList [])

                    expected =
                        Dict.fromList [ ( "a", NonEmptyList.pure A ) ]
                in
                expect actual to equal expected
            , it "should be able to update an existing value" <|
                let
                    actual =
                        updateDict "a" A (Dict.fromList [ ( "a", NonEmptyList.pure A ) ])

                    expected =
                        Dict.fromList [ ( "a", NonEmptyList.fromTuple ( A, [ A ] ) ) ]
                in
                expect actual to equal expected
            ]
        , describe "Checkout groupBy"
            [ it "should be able to group 1 sku" <|
                let
                    actual =
                        groupBy [ A ] Dict.empty

                    expected =
                        Dict.fromList [ ( "a", NonEmptyList.pure A ) ]
                in
                expect actual to equal expected
            , it "should be able to group skus" <|
                let
                    actual =
                        groupBy [ A, A, B, A, C, D, D ] Dict.empty

                    expected =
                        Dict.fromList
                            [ ( "a", NonEmptyList.fromTuple ( A, [ A, A ] ) )
                            , ( "b", NonEmptyList.pure B )
                            , ( "c", NonEmptyList.pure C )
                            , ( "d", NonEmptyList.fromTuple ( D, [ D ] ) )
                            ]
                in
                expect actual to equal expected
            ]
        ]
