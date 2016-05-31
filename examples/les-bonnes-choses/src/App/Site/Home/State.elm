module App.Site.Home.State exposing (..)

import App.Site.Home.Types exposing (..)
import App.Documents.Decoders as Documents
import App.Documents.Types as Documents
import Basics.Extra exposing (never)
import Prismic as P
import Prismic.Types as P
import Task


init : P.Cache -> ( Model, Cmd Msg )
init prismic =
    ( { products = Ok []
      , featured = Ok []
      , category = Documents.Macaron
      }
    , Cmd.batch
         -- TODO: Avoid fetching the api twice.
        [ P.fetchApi prismic
            |> P.form "products"
            |> P.submit Documents.decodeProduct
            |> Task.toResult
            |> Task.perform never SetProducts
        , P.fetchApi prismic
            |> P.form "featured"
            |> P.query [P.atL "document.tags" ["Featured"], P.at "document.type" "product"]
            |> P.submit Documents.decodeProduct
            |> Task.toResult
            |> Task.perform never SetFeatured
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
update msg model =
    case msg of
        SetProducts result ->
            case result of
                Err error ->
                    ( { model
                        | products = Err error
                      }
                    , Cmd.none
                    , Nothing
                    )

                Ok ( response, prismic ) ->
                    ( { model
                        | products = Ok (List.map .data response.results)
                      }
                    , Cmd.none
                    , Just prismic
                    )

        SetFeatured result ->
            case result of
                Err error ->
                    ( { model
                        | featured = Err error
                      }
                    , Cmd.none
                    , Nothing
                    )

                Ok ( response, prismic ) ->
                    ( { model
                        | featured = Ok (List.map .data response.results)
                      }
                    , Cmd.none
                    , Just prismic
                    )

        SetCategory category ->
            ( { model
                | category = category
              }
            , Cmd.none
            , Nothing
            )
