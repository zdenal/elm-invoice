module Product exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Round


-- ENCODERS/DECODERS


productsDecoder : Decode.Decoder (List Product)
productsDecoder =
    Decode.list productDecoder


productDecoder : Decode.Decoder Product
productDecoder =
    decode Product
        |> Json.Decode.Pipeline.required "id" Decode.int
        |> Json.Decode.Pipeline.required "item" Decode.string
        |> Json.Decode.Pipeline.required "unit" Decode.int
        |> Json.Decode.Pipeline.required "tax" Decode.float
        |> Json.Decode.Pipeline.required "price" Decode.float



-- COMMANDS


fetchProducts : (Result Http.Error (List Product) -> msg) -> Cmd msg
fetchProducts msg =
    let
        productsUrl =
            "http://localhost:4000/products"
    in
        productsDecoder
            |> Http.get productsUrl
            |> Http.send msg



-- MODEL


type alias Config msg =
    { changeUnit : Int -> String -> msg
    , changeTax : Int -> String -> msg
    , changePrice : Int -> String -> msg
    , addProduct : msg
    }


type alias Product =
    { id : Int
    , item : String
    , unit : Int
    , tax : Float
    , price : Float
    }


updateProducts : (List Product -> List Product) -> List Product -> List Product
updateProducts updateWith products =
    updateWith products


addNewProduct : List Product -> List Product
addNewProduct products =
    products
        ++ [ { id = List.length products, item = "", unit = 1, tax = 21, price = 0 } ]


calculateProductTotal : Product -> Float
calculateProductTotal product =
    calculateProductPriceTotal product + calculateProductTax product


calculateProductPriceTotal : Product -> Float
calculateProductPriceTotal { unit, price } =
    toFloat unit * price


calculateProductTax : Product -> Float
calculateProductTax ({ tax } as product) =
    calculateProductPriceTotal product * (tax / 100)



-- VIEW


viewProduct : Config msg -> Product -> Html msg
viewProduct config product =
    tr [ class "item" ]
        [ td [] [ product.id |> toString |> text ]
        , td [] [ input [ placeholder "Name of product" ] [] ]
        , td [] [ input [ value (product.unit |> toString), type_ "number", onInput (config.changeUnit product.id) ] [] ]
        , td [] [ input [ value (product.tax |> toString), type_ "number", onInput (config.changeTax product.id) ] [] ]
        , td [] [ input [ value (product.price |> toString), type_ "number", onInput (config.changePrice product.id) ] [] ]
        , td [] [ calculateProductTotal product |> Round.round 2 |> text ]
        ]


viewProducts : Config msg -> List Product -> Html msg
viewProducts config products =
    div [ class "panel-default" ]
        [ div [ class "panel-heading" ]
            [ h3 [ class "panel-title" ] [ text "Services / Products" ] ]
        , table [ class "table table-bordered table-condensed" ]
            [ tbody [] <|
                tr []
                    [ td [] [ text "Id" ]
                    , td [] [ text "Product" ]
                    , td [] [ text "Unit" ]
                    , td [] [ text "Tax" ]
                    , td [] [ text "Price" ]
                    , td [] [ text "Total" ]
                    ]
                    :: List.map (viewProduct config) products
            ]
        , button [ onClick config.addProduct, class "btn btn-primary btn-lg btn-block" ] [ text "Add a product" ]
        ]
