module App.Site.Selections.State exposing (..)

import App.Site.Selections.Types exposing (..)
import App.Site.Selections.Show.State as Show
import Prismic.Types as P


init : P.Cache -> Page -> ( Model, Cmd Msg )
init prismic page =
    case page of
        ShowP docId _ ->
            let
                ( selection, showCmd ) =
                    Show.init prismic docId
            in
                ( { page = page
                  , content = ShowC selection
                  }
                , Cmd.map ShowMsg showCmd
                )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
update msg model =
    case msg of
        ShowMsg showMsg ->
            case model.content of
                ShowC selection ->
                    let
                        ( newSelection, showCmd, mNewPrismic ) =
                            Show.update showMsg selection
                    in
                        ( { model | content = ShowC newSelection }
                        , Cmd.map ShowMsg showCmd
                        , mNewPrismic
                        )

                _ ->
                    ( model, Cmd.none, Nothing )
