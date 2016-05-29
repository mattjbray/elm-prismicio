module App.Site.Products.Index.State exposing (..)

import App.Site.Products.Index.Types exposing (..)
import App.Documents.Decoders as Documents
import Prismic.Types as P
import Prismic as P
import Task


init : P.Cache -> ( Model, Cmd Msg )
init prismic =
    ( { products = Nothing
      , error = Nothing
      }
    , prismic
        |> P.fetchApi
        |> P.form "products"
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

        SetResponse ( response, cache ) ->
            let
                newModel =
                    { model
                        | products = Just (List.map .data response.results)
                    }
            in
                ( newModel, Cmd.none, Just cache )
