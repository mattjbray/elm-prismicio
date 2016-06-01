module App.Site.Stores.State exposing (..)

import App.Site.Stores.Types exposing (..)
import App.Site.Stores.Index.State as Index
import App.Site.Stores.Show.State as Show
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


update : Msg -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
update msg model =
    case msg of
        IndexMsg indexMsg ->
            case model.content of
                IndexC index ->
                    let
                        ( newIndex, indexCmd, mNewPrismic ) =
                            Index.update indexMsg index
                    in
                        ( { model | content = IndexC newIndex }
                        , Cmd.map IndexMsg indexCmd
                        , mNewPrismic
                        )

                _ ->
                    ( model, Cmd.none, Nothing )

        ShowMsg showMsg ->
            case model.content of
                ShowC store ->
                    let
                        ( newStore, showCmd, mNewPrismic ) =
                            Show.update showMsg store
                    in
                        ( { model | content = ShowC newStore }
                        , Cmd.map ShowMsg showCmd
                        , mNewPrismic
                        )

                _ ->
                    ( model, Cmd.none, Nothing )
