module App.Site.Jobs.Index.State exposing (..)

import App.Site.Jobs.Index.Types exposing (..)
import App.Site.Article.State as Article
import App.Documents.Decoders as Documents
import App.Types exposing (GlobalMsg(SetPrismic))
import Basics.Extra exposing (never)
import Prismic as P exposing (Url(Url))
import Prismic as P
import Task


init : P.Model -> ( Model, Cmd Msg )
init prismic =
    let
        (article, articleCmd) =
          Article.init prismic "jobs"

        model =
            { article =
                article
            , jobs =
                Ok []
            }
    in
        ( model
        , Cmd.batch
            [ Cmd.map ArticleMsg articleCmd
            , P.api prismic
                |> P.form "jobs"
                |> P.submit Documents.decodeJobOffer
                |> Task.toResult
                |> Task.perform never SetJobs
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

        SetJobs (Err error) ->
            ( { model
                | jobs = Err error
              }
            , Cmd.none
            , []
            )

        SetJobs (Ok ( response, prismic )) ->
            ( { model
                | jobs =
                    response.results
                        |> List.map .data
                        |> Ok
              }
            , Cmd.none
            , [ SetPrismic prismic ]
            )
