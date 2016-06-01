module App.Site.Stores.Index.State exposing (..)

import App.Site.Stores.Index.Types exposing (..)
import App.Documents.Decoders as Documents
import Basics.Extra exposing (never)
import Prismic.Types as P exposing (Url(Url))
import Prismic as P
import Task


init : P.Cache -> ( Model, Cmd Msg )
init prismic =
    let
        model =
            { article =
                Ok Nothing
            , stores =
                Ok []
            }
    in
        ( model
        , Cmd.batch
            [ prismic
                |> P.fetchApi
                |> P.bookmark "stores"
                |> P.submit Documents.decodeArticle
                |> Task.toResult
                |> Task.perform never SetArticle
            , prismic
                |> P.fetchApi
                |> P.form "stores"
                |> P.submit Documents.decodeStore
                |> Task.toResult
                |> Task.perform never SetStores
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
update msg model =
    case msg of
        SetArticle (Err error) ->
            ( { model
                | article = Err error
              }
            , Cmd.none
            , Nothing
            )

        SetArticle (Ok ( response, prismic )) ->
            ( { model
                | article =
                    response.results
                        |> List.head
                        |> Maybe.map .data
                        |> Ok
              }
            , Cmd.none
            , Just prismic
            )

        SetStores (Err error) ->
            ( { model
                | stores = Err error
              }
            , Cmd.none
            , Nothing
            )

        SetStores (Ok ( response, prismic )) ->
            ( { model
                | stores =
                    response.results
                        |> List.map .data
                        |> Ok
              }
            , Cmd.none
            , Just prismic
            )
