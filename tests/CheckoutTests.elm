module CheckoutTests exposing (..)

import Checkout exposing (..)
import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Test exposing (..)


skus : Test
skus =
    describe "working with skus"
        [ it "should be able to convert a real SKUs" <|
            expect (stringToSKU "a") to equal (Just A)
        , it "should not convert an invalid SKU" <|
            expect (stringToSKU "x") to equal Nothing
        , it "should be able to convert a list of SKUs" <|
            let
                expected =
                    [ Just A, Just B, Just C, Just D, Nothing, Nothing ]

                actual =
                    List.map stringToSKU [ "A", "b", "C", "d", "x", "Z" ]
            in
            expect expected to equal actual
        ]
