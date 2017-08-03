module Main exposing (main)

import Documents.Homepage exposing (Homepage, decodeHomepage)
import Documents.Menu exposing (Menu, decodeMenu)
import Documents.Page exposing (decodePage)
import Html exposing (Html)
import Html.Attributes exposing (class, href, src, target)
import Html.Events exposing (onClick)
import Pages.Homepage
import Pages.Page
import Prismic
import Prismic.Api as Prismic
import Prismic.Document as Prismic
import Prismic.Url exposing (Url(Url))
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias Model =
    { prismic : Prismic.Model
    , doc :
        Maybe Homepage
    , menu : Maybe Menu
    , page : Page
    , pageDoc : Maybe Documents.Page.Page
    }


type Page
    = Homepage
    | Page


init : ( Model, Cmd Msg )
init =
    let
        model =
            { prismic =
                Prismic.init (Url "https://mattjbray-testing.prismic.io/api")
            , doc = Nothing
            , menu = Nothing
            , page = Homepage
            , pageDoc = Nothing
            }
    in
    ( model, fetchHomePage model.prismic )


type alias PrismicResult a =
    Result Prismic.PrismicError ( Prismic.Response a, Prismic.Model )


type Msg
    = HomepageResponse (PrismicResult Homepage)
    | MenuResponse (PrismicResult Menu)
    | PageResponse (PrismicResult Documents.Page.Page)
    | NavigateTo Prismic.DocumentReference


linkResolver : Prismic.DocumentReference -> List (Html.Attribute Msg)
linkResolver ref =
    [ onClick (NavigateTo ref), href "#" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomepageResponse (Ok ( result, prismic )) ->
            let
                newPrismic =
                    Prismic.cache model.prismic prismic
            in
            ( { model
                | prismic =
                    newPrismic
                , doc =
                    result.results
                        |> List.head
                        |> Maybe.map .data
              }
            , fetchMenu newPrismic
            )

        HomepageResponse (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
            model ! []

        MenuResponse (Ok ( result, prismic )) ->
            ( { model
                | prismic =
                    Prismic.cache model.prismic prismic
                , menu =
                    result.results
                        |> List.head
                        |> Maybe.map .data
              }
            , Cmd.none
            )

        MenuResponse (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
            model ! []

        PageResponse (Ok ( result, prismic )) ->
            ( { model
                | prismic =
                    Prismic.cache model.prismic prismic
                , pageDoc =
                    result.results
                        |> List.head
                        |> Maybe.map .data
              }
            , Cmd.none
            )

        PageResponse (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
            model ! []

        NavigateTo ref ->
            case (ref.linkedDocumentType, ref.uid) of
                ("homepage", Just "homepage") ->
                    ( { model | page = Homepage }, Cmd.none )

                ("page", Just uid) ->
                    ( { model | page = Page, pageDoc = Nothing }
                    , fetchPage model.prismic uid
                    )

                _ ->
                    ( model, Cmd.none )


fetchHomePage : Prismic.Model -> Cmd Msg
fetchHomePage prismic =
    Prismic.api prismic
        |> Prismic.form "everything"
        |> Prismic.query [ Prismic.at "my.homepage.uid" "homepage" ]
        |> Prismic.submit decodeHomepage
        |> Task.attempt HomepageResponse


fetchMenu : Prismic.Model -> Cmd Msg
fetchMenu prismic =
    Prismic.api prismic
        |> Prismic.form "everything"
        |> Prismic.query [ Prismic.at "my.menu.uid" "main-nav" ]
        |> Prismic.submit decodeMenu
        |> Task.attempt MenuResponse


fetchPage : Prismic.Model -> String -> Cmd Msg
fetchPage prismic uid =
    Prismic.api prismic
        |> Prismic.form "everything"
        |> Prismic.query [ Prismic.at "my.page.uid" uid ]
        |> Prismic.submit decodePage
        |> Task.attempt PageResponse


view : Model -> Html Msg
view model =
    Html.div []
        [ case model.page of
            Homepage ->
                Maybe.map2 (Pages.Homepage.view linkResolver)
                    model.menu
                    model.doc
                    |> Maybe.withDefault loading

            Page ->
                Maybe.map2 (Pages.Page.view linkResolver)
                    model.menu
                    model.pageDoc
                    |> Maybe.withDefault loading
        , viewFooter
        ]


loading : Html msg
loading =
    Html.text "..."


viewFooter : Html msg
viewFooter =
    Html.footer []
        [ Html.p []
            [ Html.text "Proudly published with "
            , Html.a [ href "https://prismic.io", target "_blank" ]
                [ Html.text "prismic.io"
                ]
            , Html.br [] []
            , Html.a [ href "https://prismic.io", target "_blank" ]
                [ Html.img
                    [ class "footer-logo"
                    , src "https://website-sample.herokuapp.com/images/logo-prismic.svg"
                    ]
                    []
                ]
            ]
        ]
