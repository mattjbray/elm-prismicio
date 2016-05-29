module App.State exposing (..)

import App.Decoders exposing (decodeMyDocument)
import App.Types exposing (..)
import App.Navigation exposing (toHash)
import App.Article.State as Article
import App.Blog.State as Blog
import Navigation
import Prismic as P
import Prismic.Types exposing (Url(Url), Cache)
import Prismic.State
import Task


initModel : Model
initModel =
    { response =
        Nothing
    , prismic =
        Prismic.State.initCache (Url "https://lesbonneschoses.prismic.io/api")
    , page =
        AboutP
    , content =
        NoContent
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NavigateTo page ->
            model
                ! if model.page == page then
                    []
                  else
                    [ Navigation.newUrl (toHash page) ]

        SetResponse ( response, cache ) ->
            ( { model
                | response = Just (Ok response)
                , prismic = cache
              }
            , Cmd.none
            )

        SetError err ->
            ( { model | response = Just (Err err) }
            , Cmd.none
            )

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


init : Result String Page -> ( Model, Cmd Msg )
init result =
    urlUpdate result initModel


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
                      , response = Nothing
                      , content = BlogC blog
                  }
          in
              newModel ! [ Cmd.map BlogMsg blogCmd ]

        Ok page ->
            let
                newModel =
                    { model
                        | page = page
                        , response = Nothing
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
                , response = Nothing
                , content = ArticleC article
            }
    in
        newModel ! [ Cmd.map ArticleMsg articleCmd ]


fetchPageFor : Model -> Cmd Msg
fetchPageFor model =
    case model.page of
        SearchP form ->
            model.prismic
                |> P.fetchApi
                |> P.form form
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse

        _ ->
            Cmd.none
