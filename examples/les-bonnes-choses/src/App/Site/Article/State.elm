module App.Site.Article.State exposing (..)

import App.Site.Article.Types exposing (..)
import App.Documents.Decoders as Documents
import Prismic.Types as P exposing (Url(Url))
import Prismic as P
import Task


init : P.Cache -> String -> ( Model, Cmd Msg )
init prismic bookmarkName =
    let
        model =
            { doc =
                Nothing
            }
    in
        ( model
        , prismic
            |> P.fetchApi
            |> P.bookmark bookmarkName
            |> P.submit Documents.decodeArticle
            |> Task.perform SetError SetResponse
        )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
update msg model =
    case msg of
        SetError _ ->
            ( model, Cmd.none, Nothing )

        SetResponse ( response, cache ) ->
            let
                newModel =
                    case List.head response.results of
                        Just result ->
                            { model
                                | doc = Just result.data
                            }

                        Nothing ->
                            model
            in
                ( newModel, Cmd.none, Just cache )
