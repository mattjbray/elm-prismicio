module App.Site.Home.State exposing (..)

import App.Site.Home.Decoders exposing (..)
import App.Site.Home.Types exposing (..)
import App.Documents.Decoders as Documents
import App.Documents.Types as Documents
import App.Types exposing (GlobalMsg(SetPrismic))
import Basics.Extra exposing (never)
import Prismic as P
import Prismic as P
import Task


init : P.Model -> ( Model, Cmd Msg )
init prismic =
    ( { products = Ok []
      , featured = Ok []
      , category = Documents.Macaron
      }
    , Cmd.batch
        [ P.api prismic
            |> P.form "products"
            |> P.submit Documents.decodeProduct
            |> Task.toResult
            |> Task.perform never SetProducts
        , P.api prismic
            |> P.form "featured"
            |> P.submit decodeFeatured
            |> Task.toResult
            |> Task.perform never SetFeatured
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        SetProducts result ->
            case result of
                Err error ->
                    ( { model
                        | products = Err error
                      }
                    , Cmd.none
                    , []
                    )

                Ok ( response, prismic ) ->
                    ( { model
                        | products = Ok (List.map .data response.results)
                      }
                    , Cmd.none
                    , [ SetPrismic prismic ]
                    )

        SetFeatured result ->
            case result of
                Err error ->
                    ( { model
                        | featured = Err error
                      }
                    , Cmd.none
                    , []
                    )

                Ok ( response, prismic ) ->
                    ( { model
                        | featured = Ok (List.map .data response.results)
                      }
                    , Cmd.none
                    , [ SetPrismic prismic ]
                    )

        SetCategory category ->
            ( { model
                | category = category
              }
            , Cmd.none
            , []
            )
