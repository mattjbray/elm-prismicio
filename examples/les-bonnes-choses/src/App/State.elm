module App.State exposing (..)

import App.Decoders exposing (decodeMyDocument)
import App.Types exposing (..)
import Prismic as P
import Prismic.Types exposing (Url(Url))
import Prismic.State
import Task


init : ( Model, Cmd Msg )
init =
    let
        model =
            { response =
                Nothing
            , prismic =
                Prismic.State.initCache (Url "https://lesbonneschoses.prismic.io/api")
            , selected =
                Bookmark "about"
            }
    in
        ( model
        , fetchSelection model
        )


fetchSelection : Model -> Cmd Msg
fetchSelection model =
    case model.selected of
        Bookmark bookmark ->
            model.prismic
                |> P.fetchApi
                |> P.bookmark bookmark
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse

        Form form ->
            model.prismic
                |> P.fetchApi
                |> P.form form
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse

        Document docId ->
            model.prismic
                |> P.fetchApi
                |> P.form "everything"
                |> P.query ("[[:d = at(document.id, \"" ++ docId ++ "\")]]")
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse

        Blog ->
            model.prismic
                |> P.fetchApi
                |> P.form "blog"
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetSelected selection ->
            let
                newModel =
                    { model
                        | selected = selection
                        , response = Nothing
                    }
            in
                ( newModel
                , fetchSelection newModel
                )

        SetResponse ( response, cache ) ->
            ( { model
                | response = Just (Ok response)
                , prismic = cache
              }
            , Cmd.none
            )

        SetError err ->
            ( { model | response = Just (Err err) }
            , Cmd.none
            )
