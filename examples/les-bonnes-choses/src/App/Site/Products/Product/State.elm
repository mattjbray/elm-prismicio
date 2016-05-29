module App.Site.Products.Product.State exposing (..)

import App.Site.Products.Product.Types exposing (..)
import App.Documents.Decoders as Documents
import Prismic.Types as P
import Prismic as P
import Task


init : P.Cache -> String -> ( Model, Cmd Msg )
init prismic docId =
    ( { product = Nothing
      , error = Nothing
      }
    , prismic
        |> P.fetchApi
        |> P.form "products"
        |> P.query (P.at "document.id" docId)
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
                ( newModel, Cmd.none, Just prismic )
