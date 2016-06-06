module App.Site.Products.State exposing (..)

import App.Site.Products.Types exposing (..)
import App.Site.Products.Index.State as Index
import App.Site.Products.Show.State as Show
import App.Types exposing (GlobalMsg(SetPrismic))
import Prismic as P


init : P.Model -> Page -> ( Model, Cmd Msg )
init prismic page =
    case page of
        IndexP mFlavour ->
            let
                ( index, indexCmd ) =
                    Index.init prismic mFlavour
            in
                ( { page = page
                  , content = IndexC index
                  }
                , Cmd.map IndexMsg indexCmd
                )

        ShowP docId _ ->
            let
                ( product, showCmd ) =
                    Show.init prismic docId
            in
                ( { page = page
                  , content = ShowC product
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
                ShowC product ->
                    let
                        ( newProduct, showCmd, globalMsgs ) =
                            Show.update showMsg product
                    in
                        ( { model | content = ShowC newProduct }
                        , Cmd.map ShowMsg showCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )
