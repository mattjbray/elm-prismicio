module Main exposing (main)

import Html.App as Html
import Html exposing (..)
import Prismic
import Prismic.Types exposing (Url(Url), Msg(FetchApiError))


type alias Model =
    { prismic : Prismic.Model
    }


type Msg
    = NoOp
    | PrismicMsg Prismic.Msg


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
    let
        ( prismicModel, prismicCmd ) =
            Prismic.init (Url "https://lesbonneschoses.prismic.io/api")
    in
        ( { prismic = prismicModel
          }
        , Cmd.map PrismicMsg prismicCmd
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PrismicMsg pMsg ->
            let
                ( prismicModel, prismicCmd ) =
                    Prismic.update pMsg model.prismic
            in
                ( { model | prismic = prismicModel }
                , Cmd.map PrismicMsg prismicCmd
                )


view : Model -> Html Msg
view model =
    p []
        [ text (toString model.prismic) ]
