module App.Site.Products.Show.State exposing (..)

import App.Site.Products.Show.Types exposing (..)
import App.Documents.Decoders as Documents
import App.Documents.Types as Documents
import App.Types exposing (GlobalMsg(SetPrismic, RenderNotFound))
import Prismic.Types as P
import Prismic as P
import Task


init : P.Cache -> String -> ( Model, Cmd Msg )
init prismic docId =
    ( { product = Nothing
      , relatedProducts = []
      , error = Nothing
      }
    , prismic
        |> P.fetchApi
        |> P.form "products"
        |> P.query [ P.at "document.id" docId ]
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

        SetResponse ( response, prismic ) ->
            case List.head response.results of
                Just result ->
                    ( { model
                        | product = Just result.data
                      }
                    , fetchRelatedProducts prismic result.data
                    , [ SetPrismic prismic ]
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    , [ SetPrismic prismic
                      , RenderNotFound
                      ]
                    )

        SetRelatedProducts ( response, prismic ) ->
            ( { model
                | relatedProducts = List.map .data response.results
              }
            , Cmd.none
            , [ SetPrismic prismic ]
            )


fetchRelatedProducts : P.Cache -> Documents.Product -> Cmd Msg
fetchRelatedProducts prismic product =
    let
        relatedDocIds =
            List.filterMap
                (\related ->
                    case related of
                        P.DocumentLink doc _ ->
                            Just doc.id

                        _ ->
                            Nothing
                )
                product.related
    in
        prismic
            |> P.fetchApi
            |> P.form "products"
            |> P.query [ P.any "document.id" relatedDocIds ]
            |> P.submit Documents.decodeProduct
            |> Task.perform SetError SetRelatedProducts
