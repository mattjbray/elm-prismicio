module App.Site.Products.Product.State exposing (..)

import App.Site.Products.Product.Types exposing (..)
import App.Documents.Decoders as Documents
import Prismic.Types as P
import Prismic as P
import Task


init : P.Cache -> String -> ( Model, Cmd Msg )
init prismic docId =
    ( { product = Nothing
      , relatedProducts = []
      , error = Nothing
      }
    , prismic
        |> P.fetchApi
        |> P.form "products"
        |> P.query [ P.at "document.id" docId ]
        |> P.submit Documents.decodeProduct
        |> Task.perform SetError SetResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
update msg model =
    case msg of
        SetError e ->
            ( { model | error = Just e }
            , Cmd.none
            , Nothing
            )

        SetResponse ( response, prismic ) ->
            let
                newModel =
                    case List.head response.results of
                        Just result ->
                            { model
                                | product = Just result.data
                            }

                        Nothing ->
                            model
            in
                fetchRelatedProducts prismic newModel

        SetRelatedProducts ( response, prismic ) ->
            ( { model
                | relatedProducts = List.map .data response.results
              }
            , Cmd.none
            , Just prismic
            )


fetchRelatedProducts : P.Cache -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
fetchRelatedProducts prismic model =
    ( model
    , case model.product of
        Just product ->
            let
                relatedDocIds =
                    List.filterMap
                        (\related ->
                            case related of
                                P.DocumentLink doc _ ->
                                    Just doc.id

                                _ ->
                                    Nothing
                        )
                        product.related
            in
                prismic
                    |> P.fetchApi
                    |> P.form "products"
                    |> P.query [ P.any "document.id" relatedDocIds ]
                    |> P.submit Documents.decodeProduct
                    |> Task.perform SetError SetRelatedProducts

        Nothing ->
            Cmd.none
    , Just prismic
    )
