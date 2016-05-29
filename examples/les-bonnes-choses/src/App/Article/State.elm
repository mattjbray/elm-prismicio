module App.Article.State exposing (..)

import App.Article.Types exposing (..)
import App.Documents.Decoders as Documents
import Prismic.Types as P exposing (Url(Url))
import Prismic.State as P
import Prismic as P
import Task


init : String -> ( Model, Cmd Msg )
init bookmarkName =
    let
        model =
            { doc =
                Nothing
            , prismic =
                P.initCache (Url "https://lesbonneschoses.prismic.io/api")
            }
    in
        ( model
        , model.prismic
            |> P.fetchApi
            |> P.bookmark bookmarkName
            |> P.submit Documents.decodeArticle
            |> Task.perform SetError SetResponse
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetError _ ->
            model ! []

        SetResponse ( response, cache ) ->
            let
                newModel =
                    { model | prismic = cache }
            in
                case List.head response.results of
                    Just result ->
                        { newModel
                            | doc = Just result.data
                        }
                            ! []

                    Nothing ->
                        newModel ! []
