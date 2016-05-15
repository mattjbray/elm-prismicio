module Prismic.State exposing (..)

import Prismic.Rest exposing (..)
import Prismic.Types exposing (..)
import Task


init : Url -> ( Model, Cmd Msg )
init url =
    ( { api = Nothing }
    , fetchApi url
        |> Task.perform FetchApiError SetApi
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetApi api ->
            ( { model | api = Just api }
            , Cmd.none
            )

        FetchApiError _ ->
            ( model, Cmd.none )
