module App.Blog.Index.State exposing (..)

import App.Blog.Index.Types exposing (..)
import App.Documents.Decoders as Documents
import Prismic.Types as P exposing (Url(Url))
import Prismic.State as P
import Prismic as P
import Task


init : ( Model, Cmd Msg )
init =
    let
        model =
            { docs =
                Nothing
            , prismic =
                P.initCache (Url "https://lesbonneschoses.prismic.io/api")
            }
    in
        ( model
        , model.prismic
            |> P.fetchApi
            |> P.form "blog"
            |> P.submit Documents.decodeBlogPost
            |> Task.perform SetError SetResponse
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetError _ ->
            model ! []

        SetResponse ( response, cache ) ->
            { model
                | prismic = cache
                , docs = Just (List.map .data response.results)
            } ! []
