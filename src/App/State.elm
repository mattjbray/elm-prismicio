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
        , selectedForm =
            "everything"
        }
    in
      ( model
      , model.prismic
          |> P.fetchApi
          |> P.form model.selectedForm
          |> P.ref "master"
          |> P.submit decodeMyDocument
          |> Task.perform SetError SetResponse
      )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetSelectedForm formName ->
            ( { model
                | selectedForm = formName
                , response = Nothing
              }
            , model.prismic
                |> P.fetchApi
                |> P.form formName
                |> P.ref "master"
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse
            )

        SetResponse (response, cache) ->
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
