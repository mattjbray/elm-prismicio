module Main exposing (main)

import Dict
import Json.Decode exposing (..)
import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (selected)
import Html.Events exposing (..)
import Prismic as P
import Prismic.Types exposing (..)
import Prismic.Decoders exposing (..)
import Prismic.View exposing (..)
import Task


type alias Model =
    { response : Maybe (Result PrismicError (Response MyDocument))
    , api : Maybe (Result PrismicError Api)
    , selectedForm : String
    }


type Msg
    = NoOp
    | SetApi Api
    | SetResponse (Response MyDocument)
    | SetError PrismicError
    | SetSelectedForm String


type MyDocument
    = Default DefaultDocType
    | JobOfferDoc JobOffer
    | BlogPostDoc BlogPost


type alias BlogPost =
    { body : StructuredText
    , author : String
    , category : String
    , date : String
    , shortLede : StructuredText
    , relatedPosts : List Link
    , relatedProducts : List Link
    , allowComments : Bool
    }


type alias JobOffer =
    { name : StructuredText
    , contractType : Maybe String
    , service : Maybe String
    , jobDescription : StructuredText
    , profile : StructuredText
    , locations : List Link
    }


decodeMyDocument : Decoder MyDocument
decodeMyDocument =
    let
        decodeOnType typeStr =
            case typeStr of
                "job-offer" ->
                    object1 JobOfferDoc decodeJobOffer

                "blog-post" ->
                    object1 BlogPostDoc decodeBlogPost

                _ ->
                    object1 Default decodeDefaultDocType
    in
        ("type" := string) `andThen` decodeOnType


decodeJobOffer : Decoder JobOffer
decodeJobOffer =
    at [ "data", "job-offer" ]
        (succeed JobOffer
            |: at [ "name", "value" ] decodeStructuredText
            |: maybe (at [ "contract_type", "value" ] string)
            |: maybe (at [ "service", "value" ] string)
            |: at [ "job_description", "value" ] decodeStructuredText
            |: at [ "profile", "value" ] decodeStructuredText
            |: at [ "location" ] (list decodeLink)
        )


decodeBlogPost : Decoder BlogPost
decodeBlogPost =
    let
        decodeAllowComments str =
            case str of
                "Yes" ->
                    succeed True

                "No" ->
                    succeed False

                _ ->
                    fail ("Unknown allow_comments value: " ++ str)
    in
        at [ "data", "blog-post" ]
            (succeed BlogPost
                |: at [ "body", "value" ] decodeStructuredText
                |: at [ "author", "value" ] string
                |: at [ "category", "value" ] string
                |: at [ "date", "value" ] string
                |: at [ "shortlede", "value" ] decodeStructuredText
                |: at [ "relatedpost" ] (list decodeLink)
                |: at [ "relatedproduct" ] (list decodeLink)
                |: at [ "allow_comments", "value" ] (string `andThen` decodeAllowComments)
            )


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


viewResponseOk : Response MyDocument -> Html msg
viewResponseOk response =
    div []
        (List.intersperse (hr [] [])
            (List.map viewDocument
                response.results
            )
        )


viewDocument : SearchResult MyDocument -> Html msg
viewDocument result =
    case result.data of
        Default doc ->
            viewDocumentGeneric doc

        JobOfferDoc doc ->
            viewDocumentJobOffer doc

        BlogPostDoc doc ->
            viewDocumentBlogPost doc


viewDocumentGeneric : DefaultDocType -> Html msg
viewDocumentGeneric doc =
    let
        allDocFields =
            let
                fieldsPerType =
                    Dict.values doc

                fieldsPerField =
                    List.concatMap Dict.values fieldsPerType
            in
                List.concat fieldsPerField
    in
        div []
            (h2 [] (List.map text (Dict.keys doc)) :: (List.map asHtml allDocFields))


viewDocumentJobOffer : JobOffer -> Html msg
viewDocumentJobOffer jobOffer =
    div []
        [ structuredTextAsHtml jobOffer.name
        , text
            (jobOffer.contractType
                |> Maybe.map (\ct -> ct ++ " position")
                |> Maybe.withDefault ""
            )
        , br [] []
        , text
            (jobOffer.service
                |> Maybe.map (\service -> service ++ " role")
                |> Maybe.withDefault ""
            )
        , structuredTextAsHtml jobOffer.jobDescription
        , structuredTextAsHtml jobOffer.profile
        ]


viewDocumentBlogPost : BlogPost -> Html msg
viewDocumentBlogPost blogPost =
    div []
        [ p [] [ text "BlogPost" ]
        , structuredTextAsHtml blogPost.body
        , em [] [ text ("Posted on " ++ blogPost.date ++ " by " ++ blogPost.author ++ " in " ++ blogPost.category) ]
        , p []
            [ text
                ("Comments are "
                    ++ (if blogPost.allowComments then
                            "enabled"
                        else
                            "disabled"
                       )
                    ++ "."
                )
            ]
        ]
