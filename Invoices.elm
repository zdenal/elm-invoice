module Invoices exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Round


main : Program Never Model Msg
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
    }


type alias Model =
    { products : List Product
    }


initModel : Model
initModel =
    Model []


addNew : Model -> Model
addNew { products } =
    products
        ++ [ { id = List.length products, item = "", unit = 1, tax = 21, price = 0 } ]
        |> Model


calculateProductTotal : Product -> Float
calculateProductTotal product =
    calculateProductPriceTotal product + calculateProductTax product


calculateProductPriceTotal : Product -> Float
calculateProductPriceTotal { unit, price } =
    toFloat unit * price


calculateProductTax : Product -> Float
calculateProductTax ({ tax } as product) =
    calculateProductPriceTotal product * (tax / 100)


updateWithId : id -> ({ a | id : id } -> { a | id : id }) -> List { a | id : id } -> List { a | id : id }
updateWithId id predicate =
    List.map (\p -> if id == p.id then predicate p else p)


updateProducts : (List Product -> List Product) -> Model -> Model
updateProducts updateWith ({ products } as model) =
    { model | products = updateWith products}


-- UPDATE


type Msg
    = AddProduct
    | ChangeUnit Int String
    | ChangeTax Int String
    | ChangePrice Int String


update : Msg -> Model -> Model
update msg ({ products } as model) =
    case msg of
        AddProduct ->
            addNew model

        ChangeUnit id newUnit ->
            let
                updateUnit product =
                    { product | unit = Result.withDefault product.unit <| String.toInt newUnit }
            in
                updateProducts (updateWithId id updateUnit) model

        ChangeTax id newTax ->
            let
                updateTax product =
                    { product | tax = Result.withDefault product.tax <| String.toFloat newTax }
            in
                updateProducts (updateWithId id updateTax) model

        ChangePrice id newPrice ->
            let
                updatePrice product =
                    { product | price = Result.withDefault product.price <| String.toFloat newPrice }
            in
                updateProducts (updateWithId id updatePrice) model



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
        , td [] [ calculateProductTotal product |> Round.round 2 |> text ]
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
            [ tbody [] <|
                tr []
                    [ td [] [ text "Id" ]
                    , td [] [ text "Product" ]
                    , td [] [ text "Unit" ]
                    , td [] [ text "Tax" ]
                    , td [] [ text "Price" ]
                    , td [] [ text "Total" ]
                    ]
                    :: List.map viewProduct products
            ]
        , button [ onClick AddProduct, class "btn btn-primary btn-lg btn-block" ] [ text "Add a product" ]
        ]
