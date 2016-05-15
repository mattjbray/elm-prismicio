module Main exposing (main)

import Dict
import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Prismic as P
import Prismic.Types exposing (Api, Response, Url(Url), PrismicError, SearchResult)
import Prismic.View exposing (asHtml, asHtmlWithDefault)
import Task


type alias Model =
    { response : Maybe (Result PrismicError Response)
    , api : Maybe (Result PrismicError Api)
    , selectedForm : String
    }


type Msg
    = NoOp
    | SetApi Api
    | SetResponse Response
    | SetError PrismicError
    | SetSelectedForm String


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
                |> P.submit
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
                        |> P.submit
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


view : Model -> Html Msg
view model =
    div []
        [ viewControls model
        , viewResponse model
        ]


viewControls : Model -> Html Msg
viewControls model =
    let
        viewOption formName =
            option [ selected (formName == model.selectedForm) ] [ text formName ]
    in
        div []
            (case model.api of
                Just (Ok api) ->
                    [ select [ onInput SetSelectedForm ]
                        (List.map viewOption (Dict.keys api.forms))
                    ]

                _ ->
                    []
            )


viewResponse : Model -> Html Msg
viewResponse model =
    case model.response of
        Nothing ->
            p [] [ text "Loading..." ]

        Just (Ok response) ->
            div []
                [ h1 [] [ text "Response" ]
                , viewResponseOk response
                ]

        Just (Err error) ->
            div []
                [ h1 [] [ text "Error" ]
                , p [] [ text (toString error) ]
                ]


viewResponseOk : Response -> Html msg
viewResponseOk response =
    div []
        (List.intersperse (hr [] [])
            (List.map viewDocument
                response.results
            )
        )


viewDocument : SearchResult -> Html msg
viewDocument result =
    case result.resultType of
        "job-offer" ->
            viewDocumentJobOffer result

        _ ->
            viewDocumentGeneric result


viewDocumentGeneric : SearchResult -> Html msg
viewDocumentGeneric result =
    let
        allDocFields =
            let
                fieldsPerType =
                    Dict.values result.data

                fieldsPerField =
                    List.concatMap Dict.values fieldsPerType
            in
                List.concat fieldsPerField
    in
        div []
            (List.map asHtml allDocFields)


viewDocumentJobOffer : SearchResult -> Html msg
viewDocumentJobOffer result =
    let
        renderField fieldName =
            asHtmlWithDefault (text ("job-offer." ++ fieldName ++ " missing"))
                "job-offer"
                fieldName
                result.data
    in
        div []
            [ renderField "name"
            , p []
                [ span []
                    [ strong [] [ text "Contract Type" ]
                    , text ": "
                    , renderField "contract_type"
                    ]
                , text " "
                , span []
                    [ strong [] [ text "Service" ]
                    , text ": "
                    , renderField "service"
                    ]
                ]
            , strong [] [ text "Location: " ]
            , renderField "location"
            , renderField "job_description"
            , renderField "profile"
            ]
