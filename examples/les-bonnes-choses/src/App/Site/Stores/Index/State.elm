module App.Site.Stores.Index.State exposing (..)

import App.Site.Stores.Index.Types exposing (..)
import App.Site.Article.State as Article
import App.Documents.Decoders as Documents
import App.Types exposing (GlobalMsg(SetPrismic))
import Basics.Extra exposing (never)
import Prismic.Types as P exposing (Url(Url))
import Prismic as P
import Task


init : P.Cache -> ( Model, Cmd Msg )
init prismic =
    let
        (article, articleCmd) =
          Article.init prismic "stores"

        model =
            { article =
                article
            , stores =
                Ok []
            }
    in
        ( model
        , Cmd.batch
            [ Cmd.map ArticleMsg articleCmd
            , prismic
                |> P.fetchApi
                |> P.form "stores"
                |> P.submit Documents.decodeStore
                |> Task.toResult
                |> Task.perform never SetStores
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        ArticleMsg articleMsg ->
          let
            (newArticle, articleCmd, globalMsgs) =
              Article.update articleMsg model.article
          in
            ( { model
                | article = newArticle
              }
            , Cmd.map ArticleMsg articleCmd
            , globalMsgs
            )

        SetStores (Err error) ->
            ( { model
                | stores = Err error
              }
            , Cmd.none
            , []
            )

        SetStores (Ok ( response, prismic )) ->
            ( { model
                | stores =
                    response.results
                        |> List.map .data
                        |> Ok
              }
            , Cmd.none
            , [ SetPrismic prismic ]
            )
