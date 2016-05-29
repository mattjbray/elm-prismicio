module App.Site.Products.State exposing (..)

import App.Site.Products.Types exposing (..)
import App.Site.Products.Index.State as Index
import App.Site.Products.Product.State as Product
import Prismic.Types as P


init : P.Cache -> Page -> (Model, Cmd Msg)
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

        ProductMsg productMsg ->
            case model.content of
                ProductC product ->
                    let
                        ( newProduct, productCmd, mNewPrismic ) =
                            Product.update productMsg product
                    in
                        ( { model | content = ProductC newProduct }
                        , Cmd.map ProductMsg productCmd
                        , mNewPrismic
                        )

                _ ->
                    ( model, Cmd.none, Nothing )
