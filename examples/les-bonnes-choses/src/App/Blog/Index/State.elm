module App.Blog.Index.State exposing (..)

import App.Blog.Index.Types exposing (..)
import App.Documents.Decoders as Documents
import Prismic.Types as P exposing (Url(Url))
import Prismic as P
import Task


init : P.Cache -> ( Model, Cmd Msg )
init prismic =
    let
        model =
            { docs =
                Nothing
            }
    in
        ( model
        , prismic
            |> P.fetchApi
            |> P.form "blog"
            |> P.submit Documents.decodeBlogPost
            |> Task.perform SetError SetResponse
        )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
update msg model =
    case msg of
        SetError _ ->
            ( model, Cmd.none, Nothing )

        SetResponse ( response, prismic ) ->
            ( { model
                | docs = Just (List.map .data response.results)
              }
            , Cmd.none
            , Just prismic
            )
