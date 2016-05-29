module App.Site.State exposing (..)

import App.Site.Types exposing (..)
import App.Site.Article.State as Article
import Prismic.Types as P


init : P.Cache -> Page -> ( Model, Cmd Msg )
init prismic page =
    let
        model =
            { page = AboutP
            , content = NoContent
            }

        initWith bookmark =
            initArticle prismic page bookmark model
    in
        case page of
            AboutP as page ->
                initWith "about"

            JobsP as page ->
                initWith "jobs"

            StoresP as page ->
                initWith "stores"

            (SearchP _) as page ->
                initWith "about"


initArticle : P.Cache -> Page -> String -> Model -> ( Model, Cmd Msg )
initArticle prismic page bookmarkName model =
    let
        ( article, articleCmd ) =
            Article.init prismic bookmarkName

        newModel =
            { model
                | page = page
                , content = ArticleC article
            }
    in
        newModel ! [ Cmd.map ArticleMsg articleCmd ]


update : Msg -> Model -> ( Model, Cmd Msg, Maybe P.Cache )
update msg model =
    case msg of
        ArticleMsg articleMsg ->
            case model.content of
                ArticleC article ->
                    let
                        ( newArticle, articleCmd, mNewPrismic ) =
                            Article.update articleMsg article

                        newModel =
                            { model
                                | content = ArticleC newArticle
                            }
                    in
                        ( newModel
                        , Cmd.map ArticleMsg articleCmd
                        , mNewPrismic
                        )

                _ ->
                    ( model, Cmd.none, Nothing )
