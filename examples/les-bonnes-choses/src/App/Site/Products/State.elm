module App.Site.Products.State exposing (..)

import App.Site.Products.Types exposing (..)
import App.Site.Products.Index.State as Index
import App.Site.Products.Product.State as Product
import App.Types exposing (GlobalMsg(SetPrismic))
import Prismic.Types as P


init : P.Cache -> Page -> ( Model, Cmd Msg )
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

        ProductP docId _ ->
            let
                ( product, productCmd ) =
                    Product.init prismic docId
            in
                ( { page = page
                  , content = ProductC product
                  }
                , Cmd.map ProductMsg productCmd
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

        ProductMsg productMsg ->
            case model.content of
                ProductC product ->
                    let
                        ( newProduct, productCmd, globalMsgs ) =
                            Product.update productMsg product
                    in
                        ( { model | content = ProductC newProduct }
                        , Cmd.map ProductMsg productCmd
                        , globalMsgs
                        )

                _ ->
                    ( model, Cmd.none, [] )
