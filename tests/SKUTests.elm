module SKUTests exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import SKU exposing (..)
import Test exposing (..)


skus : Test
skus =
    describe "Working with skus"
        [ it "should be able to convert a real SKUs" <|
            expect (SKU.fromString "a") to equal (Just A)
        , it "should not convert an invalid SKU" <|
            expect (SKU.fromString "x") to equal Nothing
        , it "should be able to convert a list of SKUs" <|
            let
                expected =
                    [ Just A, Just B, Just C, Just D, Nothing, Nothing ]

                actual =
                    List.map SKU.fromString [ "A", "b", "C", "d", "x", "Z" ]
            in
            expect expected to equal actual
        ]
