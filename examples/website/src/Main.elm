module Main exposing (main)

import Documents.Homepage
import Documents.Menu
import Documents.Page
import Html exposing (Html)
import Html.Attributes exposing (class, href, src, target)
import Html.Events exposing (onClick)
import Pages.Homepage
import Pages.Page
import Prismic
import Prismic.Field as Prismic exposing (defaultLinkResolver)
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
    , currentPage : Page
    , menu : Maybe Documents.Menu.Menu
    , homepage : Maybe Documents.Homepage.Homepage
    , page : Maybe Documents.Page.Page
    }


type Page
    = Homepage
    | Page


init : ( Model, Cmd Msg )
init =
    let
        model =
            { prismic =
                Prismic.init "https://mattjbray-testing.prismic.io/api"
            , currentPage = Homepage
            , menu = Nothing
            , homepage = Nothing
            , page = Nothing
            }
    in
    ( model, fetchHomePage model.prismic )


type alias PrismicResult a =
    Result Prismic.PrismicError ( Prismic.Response a, Prismic.Model )


type Msg
    = HomepageResponse (PrismicResult Documents.Homepage.Homepage)
    | MenuResponse (PrismicResult Documents.Menu.Menu)
    | PageResponse (PrismicResult Documents.Page.Page)
    | NavigateTo Prismic.DocumentReference


linkResolver : Prismic.LinkResolver Msg
linkResolver =
    { defaultLinkResolver
        | resolveDocumentReference =
            \ref ->
                [ onClick (NavigateTo ref), href "#" ]
    }


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
                , homepage =
                    List.head result.results
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
                , page =
                    List.head result.results
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
            case ( ref.linkedDocumentType, ref.uid ) of
                ( "homepage", Just "homepage" ) ->
                    ( { model | currentPage = Homepage }, Cmd.none )

                ( "page", Just uid ) ->
                    ( { model | currentPage = Page, page = Nothing }
                    , fetchPage model.prismic uid
                    )

                _ ->
                    ( model, Cmd.none )


fetchHomePage : Prismic.Model -> Cmd Msg
fetchHomePage prismic =
    Prismic.api prismic
        |> Prismic.form "everything"
        |> Prismic.query [ Prismic.at "my.homepage.uid" "homepage" ]
        |> Prismic.submit Documents.Homepage.decodeHomepage
        |> Task.attempt HomepageResponse


fetchMenu : Prismic.Model -> Cmd Msg
fetchMenu prismic =
    Prismic.api prismic
        |> Prismic.form "everything"
        |> Prismic.query [ Prismic.at "my.menu.uid" "main-nav" ]
        |> Prismic.submit Documents.Menu.decodeMenu
        |> Task.attempt MenuResponse


fetchPage : Prismic.Model -> String -> Cmd Msg
fetchPage prismic uid =
    Prismic.api prismic
        |> Prismic.form "everything"
        |> Prismic.query [ Prismic.at "my.page.uid" uid ]
        |> Prismic.submit Documents.Page.decodePage
        |> Task.attempt PageResponse


view : Model -> Html Msg
view model =
    Html.div []
        [ case model.currentPage of
            Homepage ->
                Maybe.map2 (Pages.Homepage.view linkResolver)
                    model.menu
                    model.homepage
                    |> Maybe.withDefault loading

            Page ->
                Maybe.map2 (Pages.Page.view linkResolver)
                    model.menu
                    model.page
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
