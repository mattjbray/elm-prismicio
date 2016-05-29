module App.State exposing (..)

import App.Types exposing (..)
import App.Navigation exposing (toHash)
import App.Article.State as Article
import App.Blog.State as Blog
import Navigation


init : Result String Page -> ( Model, Cmd Msg )
init result =
    let
        model =
            { page =
                AboutP
            , content =
                NoContent
            }
    in
        urlUpdate result model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo page ->
            model
                ! if model.page == page then
                    []
                  else
                    [ Navigation.newUrl (toHash page) ]

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

        BlogMsg blogMsg ->
            case model.content of
                BlogC blog ->
                    let
                        ( newBlog, blogCmd ) =
                            Blog.update blogMsg blog
                    in
                        ( { model | content = BlogC newBlog }
                        , Cmd.map BlogMsg blogCmd
                        )

                _ ->
                    model ! []


urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case Debug.log "result" result of
        Err _ ->
            ( model, Navigation.modifyUrl (toHash model.page) )

        Ok (AboutP as page) ->
            initArticle page "about" model

        Ok (JobsP as page) ->
            initArticle page "jobs" model

        Ok (StoresP as page) ->
            initArticle page "stores" model

        Ok ((BlogP blogPage) as page) ->
            let
                ( blog, blogCmd ) =
                    Blog.init blogPage

                newModel =
                    { model
                        | page = page
                        , content = BlogC blog
                    }
            in
                newModel ! [ Cmd.map BlogMsg blogCmd ]

        Ok page ->
            let
                newModel =
                    { model
                        | page = page
                        , content = NoContent
                    }
            in
                newModel ! []


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
