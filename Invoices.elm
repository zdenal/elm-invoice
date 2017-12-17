module Invoices exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Round


main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }



-- MODEL


type alias Product =
    { id : Int
    , item : String
    , unit : Int
    , tax : Float
    , price : Float
    , total : Float
    }


type alias Model =
    { total : Float
    , products : List Product
    }


initModel : Model
initModel =
    Model 0 []


createProduct : Int -> Product
createProduct id =
    Product id "" 1 21 0.0 0.0


calculateProductTotal : Product -> Float
calculateProductTotal product =
    (calculateProductPriceTotal product) + (calculateProductTax product)


calculateProductPriceTotal : Product -> Float
calculateProductPriceTotal product =
    let
        unit =
            toFloat product.unit
    in
        unit * product.price


calculateProductTax : Product -> Float
calculateProductTax product =
    (calculateProductPriceTotal product) * (product.tax / 100)


calculateModelTotal : List Product -> Float
calculateModelTotal products =
    let
        total =
            List.map .total products |> List.sum
    in
        total



-- UPDATE


type Msg
    = AddProduct
    | ChangeUnit Int String
    | ChangeTax Int String
    | ChangePrice Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddProduct ->
            { model | products = model.products ++ [ model.products |> List.length |> createProduct ] }

        ChangeUnit id newUnit ->
            let
                changeUnit product =
                    if product.id == id then
                        { product
                            | unit = Result.withDefault product.unit <| String.toInt newUnit
                        }
                    else
                        product
            in
                { model
                    | products = List.map changeUnit model.products
                    , total = calculateModelTotal model.products
                }

        ChangeTax id newTax ->
            let
                changeTax product =
                    if product.id == id then
                        { product
                            | tax = Result.withDefault product.tax <| String.toFloat newTax
                        }
                    else
                        product
            in
                { model
                    | products = List.map changeTax model.products
                }

        ChangePrice id newPrice ->
            let
                changePrice product =
                    if product.id == id then
                        { product
                            | price = Result.withDefault product.price <| String.toFloat newPrice
                        }
                    else
                        product
            in
                { model
                    | products = List.map changePrice model.products
                }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewProducts model.products
        , br [] []
        , viewSummary model
        ]


viewProduct : Product -> Html Msg
viewProduct product =
    tr [ class "item" ]
        [ td [] [ product.id |> toString |> text ]
        , td [] [ input [ placeholder "Name of product" ] [] ]
        , td [] [ input [ value (product.unit |> toString), type_ "number", onInput (ChangeUnit product.id) ] [] ]
        , td [] [ input [ value (product.tax |> toString), type_ "number", onInput (ChangeTax product.id) ] [] ]
        , td [] [ input [ value (product.price |> toString), type_ "number", onInput (ChangePrice product.id) ] [] ]
        , td [] [ calculateProductTotal product |> toString |> text ]
        ]


viewSummary : Model -> Html Msg
viewSummary model =
    let
        unitTotal =
            List.map .unit model.products |> List.sum |> toString

        priceTotal =
            List.map calculateProductPriceTotal model.products |> List.sum |> Round.round 2

        taxTotal =
            List.map calculateProductTax model.products |> List.sum |> Round.round 2

        total =
            List.map calculateProductTotal model.products |> List.sum |> Round.round 2
    in
        div [ class "panel panel-default" ]
            [ table [ class "table table-bordered table-condensed" ]
                [ thead []
                    [ tr []
                        [ td [ class "text-center col-xs-1" ] [ text "Unit Total" ]
                        , td [ class "text-center col-xs-1" ] [ text "Tax Total" ]
                        , td [ class "text-center col-xs-1" ] [ text "Price Total" ]
                        , td [ class "text-center col-xs-1" ] [ text "Total" ]
                        ]
                    ]
                , tbody []
                    [ tr []
                        [ td [ class "text-center rowtotal mono" ] [ text unitTotal ]
                        , td [ class "text-center rowtotal mono" ] [ text taxTotal ]
                        , td [ class "text-center rowtotal mono" ] [ text priceTotal ]
                        , td [ class "text-center rowtotal mono" ] [ text total ]
                        ]
                    ]
                ]
            ]


viewProducts : List Product -> Html Msg
viewProducts products =
    div [ class "panel-default" ]
        [ div [ class "panel-heading" ]
            [ h3 [ class "panel-title" ] [ text "Services / Products" ] ]
        , table [ class "table table-bordered table-condensed" ]
            [ tbody []
                ([ tr []
                    [ td [] [ text "Id" ]
                    , td [] [ text "Product" ]
                    , td [] [ text "Unit" ]
                    , td [] [ text "Tax" ]
                    , td [] [ text "Price" ]
                    , td [] [ text "Total" ]
                    ]
                 ]
                    ++ (List.map viewProduct products)
                )
            ]
        , button [ onClick AddProduct, class "btn btn-primary btn-lg btn-block" ] [ text "Add a product" ]
        ]
