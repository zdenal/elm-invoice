module Invoices exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Round
import Product


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { products : List Product.Product
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( initModel, Product.fetchProducts NewProducts )


initModel : Model
initModel =
    Model []


updateWithId : id -> ({ a | id : id } -> { a | id : id }) -> List { a | id : id } -> List { a | id : id }
updateWithId id predicate =
    List.map
        (\p ->
            if id == p.id then
                predicate p
            else
                p
        )



-- UPDATE


type Msg
    = AddProduct
    | ChangeUnit Int String
    | ChangeTax Int String
    | ChangePrice Int String
    | NewProducts (Result Http.Error (List Product.Product))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ products } as model) =
    case msg of
        NewProducts (Ok products) ->
            let
                _ =
                    Debug.log "Result: " products
            in
                ( { model | products = products }, Cmd.none )

        NewProducts (Err error) ->
            let
                _ =
                    Debug.log "Error: " error
            in
                ( model, Cmd.none )

        AddProduct ->
            ( { model | products = Product.addNewProduct model.products }
            , Cmd.none
            )

        ChangeUnit id newUnit ->
            let
                updateUnit product =
                    { product | unit = Result.withDefault product.unit <| String.toInt newUnit }
            in
                ( { model | products = Product.updateProducts (updateWithId id updateUnit) model.products }
                , Cmd.none
                )

        ChangeTax id newTax ->
            let
                updateTax product =
                    { product | tax = Result.withDefault product.tax <| String.toFloat newTax }
            in
                ( { model | products = Product.updateProducts (updateWithId id updateTax) model.products }
                , Cmd.none
                )

        ChangePrice id newPrice ->
            let
                updatePrice product =
                    { product | price = Result.withDefault product.price <| String.toFloat newPrice }
            in
                ( { model | products = Product.updateProducts (updateWithId id updatePrice) model.products }
                , Cmd.none
                )



-- VIEW


view : Model -> Html Msg
view model =
    let
        config =
            { changeUnit = ChangeUnit
            , changeTax = ChangeTax
            , changePrice = ChangePrice
            , addProduct = AddProduct
            }
    in
        div []
            [ Product.viewProducts config model.products
            , br [] []
            , viewSummary model
            ]


viewSummary : Model -> Html Msg
viewSummary model =
    let
        unitTotal =
            List.map .unit model.products |> List.sum |> toString

        priceTotal =
            List.map Product.calculateProductPriceTotal model.products |> List.sum |> Round.round 2

        taxTotal =
            List.map Product.calculateProductTax model.products |> List.sum |> Round.round 2

        total =
            List.map Product.calculateProductTotal model.products |> List.sum |> Round.round 2
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
