module App.Site.State exposing (..)

import App.Site.Types exposing (..)
import App.Site.Article.State as Article


init : Page -> ( Model, Cmd Msg )
init page =
    let
        model =
            { page = AboutP
            , content = NoContent
            }
    in
        case page of
            AboutP as page ->
                initArticle page "about" model

            JobsP as page ->
                initArticle page "jobs" model

            StoresP as page ->
                initArticle page "stores" model

            (SearchP _) as page ->
                initArticle page "about" model


initArticle : Page -> String -> Model -> ( Model, Cmd Msg )
initArticle page bookmarkName model =
    let
        ( article, articleCmd ) =
            Article.init bookmarkName

        newModel =
            { model
                | page = page
                , content = ArticleC article
            }
    in
        newModel ! [ Cmd.map ArticleMsg articleCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArticleMsg articleMsg ->
            case model.content of
                ArticleC article ->
                    let
                        ( newArticle, articleCmd ) =
                            Article.update articleMsg article
                    in
                        ( { model | content = ArticleC newArticle }
                        , Cmd.map ArticleMsg articleCmd
                        )

                _ ->
                    model ! []
