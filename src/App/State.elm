module App.State exposing (..)

import App.Decoders exposing (decodeMyDocument)
import App.Types exposing (..)
import Prismic as P
import Prismic.Types exposing (Url(Url))
import Task


init : ( Model, Cmd Msg )
init =
    ( { response = Nothing
      , api = Nothing
      , selectedForm = "everything"
      }
    , P.init (Url "https://lesbonneschoses.prismic.io/api")
        |> Task.perform SetError SetApi
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetApi api ->
            ( { model | api = Just (Ok api) }
            , Task.succeed api
                |> P.form model.selectedForm
                |> P.withRef "master"
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse
            )

        SetSelectedForm formName ->
            ( { model
                | selectedForm = formName
                , response = Nothing
              }
            , case model.api of
                Just (Ok api) ->
                    Task.succeed api
                        |> P.form formName
                        |> P.withRef "master"
                        |> P.submit decodeMyDocument
                        |> Task.perform SetError SetResponse

                _ ->
                    Cmd.none
            )

        SetResponse response ->
            ( { model | response = Just (Ok response) }
            , Cmd.none
            )

        SetError err ->
            ( { model | response = Just (Err err) }
            , Cmd.none
            )
