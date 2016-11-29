module App.Site.Products.Show.State exposing (..)

import App.Site.Products.Show.Types exposing (..)
import App.Common exposing (performCompat)
import App.Documents.Decoders as Documents
import App.Documents.Types as Documents
import App.Types exposing (GlobalMsg(SetPrismic, RenderNotFound))
import Prismic as P


init : P.Model -> String -> ( Model, Cmd Msg )
init prismic docId =
    ( { product = Nothing
      , relatedProducts = []
      , error = Nothing
      }
    , P.api prismic
        |> P.form "products"
        |> P.query [ P.at "document.id" docId ]
        |> P.submit Documents.decodeProduct
        |> performCompat SetError SetResponse
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


fetchRelatedProducts : P.Model -> Documents.Product -> Cmd Msg
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
        P.api prismic
            |> P.form "products"
            |> P.query [ P.any "document.id" relatedDocIds ]
            |> P.submit Documents.decodeProduct
            |> performCompat SetError SetRelatedProducts
