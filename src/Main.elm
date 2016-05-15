module Main exposing (main)

import Html.App as Html
import Html exposing (..)
import Http
import Prismic
import Prismic.Types as Prismic
import Task


type alias Model =
    { api : Maybe (Result Http.Error Prismic.Api)
    , results : Maybe Prismic.Response
    , error : String
    }


type Msg
    = NoOp
    | SetApi Prismic.Api
    | FetchApiError Http.Error
    | FetchFormError Prismic.FetchFormError
    | SetResults Prismic.Response


main : Program Never
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { api = Nothing
      , results = Nothing
      , error = ""
      }
    , fetchApi
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetApi api ->
            ( { model | api = Just (Ok api) }
            , Prismic.fetchForm api "master" "everything"
                |> Task.perform FetchFormError SetResults
            )

        FetchApiError err ->
            ( { model | api = Just (Err err) }
            , Cmd.none
            )

        FetchFormError err ->
            ( { model | error = toString err }
            , Cmd.none
            )

        SetResults results ->
            ( { model | results = Just results }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    p []
        [ text (toString model.results ++ toString model.error) ]


fetchApi : Cmd Msg
fetchApi =
    Prismic.fetchApi "https://lesbonneschoses.prismic.io/api"
        |> Task.perform FetchApiError SetApi
