module App.Site.Search.Results.State exposing (..)

import App.Site.Search.Results.Decoders exposing (..)
import App.Site.Search.Results.Types exposing (..)
import App.Types exposing (GlobalMsg(SetPrismic))
import Basics.Extra exposing (never)
import Dict
import Prismic as P
import Task


init : P.Model -> String -> ( Model, Cmd Msg )
init prismic query =
    ( { products = Ok []
      , articles = Ok []
      , bookmarks = Dict.empty
      }
    , Cmd.batch
        [ P.api prismic
            |> P.form "everything"
            |> P.query
                [ P.any "document.type" [ "product", "selection" ]
                , P.fulltext "document" query
                ]
            |> P.submit decodeProductR
            |> Task.toResult
            |> Task.perform never SetProducts
        , P.api prismic
            |> P.form "everything"
            |> P.query
                [ P.any "document.type"
                    [ "blog-post"
                    , "article"
                    , "job-offer"
                    , "store"
                    ]
                , P.fulltext "document" query
                ]
            |> P.submit decodeArticleR
            |> Task.toResult
            |> Task.perform never SetArticles
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
                        | products =
                            response.results
                                |> List.map .data
                                |> Ok
                      }
                    , Cmd.none
                    , [ SetPrismic prismic ]
                    )

        SetArticles result ->
            case result of
                Err error ->
                    ( { model
                        | articles = Err error
                      }
                    , Cmd.none
                    , []
                    )

                Ok ( response, prismic ) ->
                    ( { model
                        | articles =
                            response.results
                                |> List.map .data
                                |> Ok
                        , bookmarks =
                            prismic.api
                                |> Maybe.map .bookmarks
                                |> Maybe.withDefault Dict.empty
                      }
                    , Cmd.none
                    , [ SetPrismic prismic ]
                    )
