module App.Site.Selections.State exposing (..)

import App.Site.Selections.Types exposing (..)
import App.Site.Selections.Show.State as Show
import App.Types exposing (GlobalMsg(SetPrismic))
import Prismic as P


init : P.Model -> Page -> ( Model, Cmd Msg )
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


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        ShowMsg showMsg ->
            case model.content of
                ShowC selection ->
                    let
                        ( newSelection, showCmd, globalMsgs ) =
                            Show.update showMsg selection
                    in
                        ( { model | content = ShowC newSelection }
                        , Cmd.map ShowMsg showCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )
