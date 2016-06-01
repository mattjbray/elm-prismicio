module App.Site.Stores.State exposing (..)

import App.Site.Stores.Types exposing (..)
import App.Site.Stores.Index.State as Index
import App.Site.Stores.Show.State as Show
import App.Types exposing (GlobalMsg(SetPrismic))
import Prismic.Types as P


init : P.Cache -> Page -> ( Model, Cmd Msg )
init prismic page =
    case page of
        IndexP ->
            let
                ( index, indexCmd ) =
                    Index.init prismic
            in
                ( { page = page
                  , content = IndexC index
                  }
                , Cmd.map IndexMsg indexCmd
                )

        ShowP docId _ ->
            let
                ( store, showCmd ) =
                    Show.init prismic docId
            in
                ( { page = page
                  , content = ShowC store
                  }
                , Cmd.map ShowMsg showCmd
                )


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        IndexMsg indexMsg ->
            case model.content of
                IndexC index ->
                    let
                        ( newIndex, indexCmd, globalMsgs ) =
                            Index.update indexMsg index
                    in
                        ( { model | content = IndexC newIndex }
                        , Cmd.map IndexMsg indexCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )

        ShowMsg showMsg ->
            case model.content of
                ShowC store ->
                    let
                        ( newStore, showCmd, globalMsgs ) =
                            Show.update showMsg store
                    in
                        ( { model | content = ShowC newStore }
                        , Cmd.map ShowMsg showCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )
