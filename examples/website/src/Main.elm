module Main exposing (main)

import Documents.Homepage exposing (Homepage, decodeHomepage)
import Documents.Menu exposing (Menu, decodeMenu)
import Html exposing (Html)
import Pages.Homepage
import Prismic
import Prismic.Api as Prismic
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
    }


type Page
    = Homepage


init : ( Model, Cmd Msg )
init =
    let
        model =
            { prismic =
                Prismic.init (Url "https://mattjbray-testing.prismic.io/api")
            , doc = Nothing
            , menu = Nothing
            , page = Homepage
            }
    in
    ( model, fetchHomePage model.prismic )



-- model ! []


type alias PrismicResult a =
    Result Prismic.PrismicError ( Prismic.Response a, Prismic.Model )


type Msg
    = HomepageResponse (PrismicResult Homepage)
    | MenuResponse (PrismicResult Menu)


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


view : Model -> Html Msg
view model =
    case model.page of
        Homepage ->
            Maybe.map2 Pages.Homepage.view
                model.menu
                model.doc
                |> Maybe.withDefault loading


loading : Html msg
loading =
    Html.text "..."
