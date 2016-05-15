module Main exposing (main)

import Dict
import Html.App as Html
import Html exposing (..)
import Prismic as P
import Prismic.Types exposing (Response, Url(Url), PrismicError)
import Prismic.View exposing (asHtml)
import Task


type alias Model =
    { response : Maybe (Result PrismicError Response)
    }


type Msg
    = NoOp
    | SetResponse Response
    | SetError PrismicError


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
    ( { response = Nothing }
    , P.init (Url "https://lesbonneschoses.prismic.io/api")
        |> P.form "everything"
        |> P.withRef "master"
        |> P.submit
        |> Task.perform SetError SetResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetResponse response ->
            ( { model | response = Just (Ok response) }
            , Cmd.none
            )

        SetError err ->
            ( { model | response = Just (Err err) }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.response of
        Nothing ->
            p [] [ text "Loading..." ]

        Just (Ok response) ->
            div []
                [ h1 [] [ text "Response" ]
                , viewResponse response
                ]

        Just (Err error) ->
            div []
                [ h1 [] [ text "Error" ]
                , p [] [ text (toString error) ]
                ]


viewResponse : Response -> Html msg
viewResponse response =
    let
        docFields result =
            let
                fieldsPerType =
                    Dict.values result.data

                fieldsPerField =
                    List.concatMap Dict.values fieldsPerType
            in
                List.concat fieldsPerField
    in
        div []
            (List.intersperse (hr [] [])
                (List.map
                    (\result ->
                        div []
                            (List.map asHtml (docFields result))
                    )
                    response.results
                )
            )
