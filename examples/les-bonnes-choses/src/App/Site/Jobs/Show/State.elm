module App.Site.Jobs.Show.State exposing (..)

import App.Site.Article.State as Article
import App.Site.Jobs.Show.Types exposing (..)
import App.Documents.Decoders as Documents
import App.Types exposing (GlobalMsg(SetPrismic, RenderNotFound))
import Basics.Extra exposing (never)
import Prismic as P
import Task


init : P.Model -> String -> ( Model, Cmd Msg )
init prismic docId =
    let
        ( article, articleCmd ) =
            Article.init prismic "jobs"

        model =
            { article =
                article
            , job =
                Ok Nothing
            }
    in
        ( model
        , Cmd.batch
            [ Cmd.map ArticleMsg articleCmd
            , P.api prismic
                |> P.form "everything"
                |> P.query [ P.at "document.id" docId ]
                |> P.submit Documents.decodeJobOffer
                |> Task.toResult
                |> Task.perform never SetJob
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        SetJob result ->
            case result of
                Err error ->
                    ( { model
                        | job = Err error
                      }
                    , Cmd.none
                    , []
                    )

                Ok ( response, prismic ) ->
                    case List.head response.results of
                        Nothing ->
                            ( { model
                                | job = Ok Nothing
                              }
                            , Cmd.none
                            , [ SetPrismic prismic
                              , RenderNotFound
                              ]
                            )

                        Just result ->
                            ( { model
                                | job = Ok (Just result.data)
                              }
                            , Cmd.none
                            , [ SetPrismic prismic ]
                            )

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
