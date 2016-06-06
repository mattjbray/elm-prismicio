module App.Site.Products.Index.State exposing (..)

import App.Site.Products.Index.Types exposing (..)
import App.Documents.Decoders as Documents
import App.Types exposing (GlobalMsg(SetPrismic))
import Prismic.Types as P
import Prismic as P
import Task


init : P.Model -> Maybe String -> ( Model, Cmd Msg )
init prismic mFlavour =
    ( { products = Nothing
      , error = Nothing
      }
    , prismic
        |> P.fetchApi
        |> P.form "products"
        |> (mFlavour
                |> Maybe.map (\flavour -> P.query [ P.at "my.product.flavour" flavour ])
                |> Maybe.withDefault P.none
           )
        |> P.submit Documents.decodeProduct
        |> Task.perform SetError SetResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        SetError e ->
            ( { model | error = Just e }
            , Cmd.none
            , []
            )

        SetResponse ( response, cache ) ->
            let
                newModel =
                    { model
                        | products = Just (List.map .data response.results)
                    }
            in
                ( newModel, Cmd.none, [ SetPrismic cache ] )
