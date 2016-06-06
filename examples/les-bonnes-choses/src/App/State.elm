module App.State exposing (..)

import App.Types exposing (..)
import App.Blog.State as Blog
import App.Site.State as Site
import App.Site.Types as Site
import Prismic as P exposing (Url(Url))


init : Result String Page -> ( Model, Cmd Msg )
init result =
    let
        model =
            { page =
                SiteP Site.AboutP
            , content =
                NoContent
            , prismic =
                P.init (Url "https://lesbonneschoses.prismic.io/api")
            }
    in
        urlUpdate result model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SiteMsg siteMsg ->
            case model.content of
                SiteC site ->
                    let
                        ( newSite, siteCmd, globalMsgs ) =
                            Site.update siteMsg site
                    in
                        ( { model
                            | content = SiteC newSite
                          }
                            |> processGlobalMsgs globalMsgs
                        , Cmd.map SiteMsg siteCmd
                        )

                _ ->
                    model ! []

        BlogMsg blogMsg ->
            case model.content of
                BlogC blog ->
                    let
                        ( newBlog, blogCmd, globalMsgs ) =
                            Blog.update blogMsg blog
                    in
                        ( { model
                            | content = BlogC newBlog
                          }
                            |> processGlobalMsgs globalMsgs
                        , Cmd.map BlogMsg blogCmd
                        )

                _ ->
                    model ! []


processGlobalMsgs : List GlobalMsg -> Model -> Model
processGlobalMsgs msgs model =
    let
        processMsg msg mod =
            case msg of
                SetPrismic prismic ->
                    { mod
                        | prismic = prismic
                    }

                RenderNotFound ->
                    { mod
                        | content = NoContent
                        , page = NotFoundP
                    }
    in
        List.foldl processMsg model msgs


urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case result of
        Err _ ->
            ( { model
                | page = NotFoundP
                , content = NoContent
              }
            , Cmd.none
            )

        Ok NotFoundP ->
            ( { model
                | page = NotFoundP
                , content = NoContent
              }
            , Cmd.none
            )

        Ok ((SiteP sitePage) as page) ->
            let
                ( site, siteCmd ) =
                    Site.init model.prismic sitePage

                newModel =
                    { model
                        | page = page
                        , content = SiteC site
                    }
            in
                newModel ! [ Cmd.map SiteMsg siteCmd ]

        Ok ((BlogP blogPage) as page) ->
            let
                ( blog, blogCmd ) =
                    Blog.init model.prismic blogPage

                newModel =
                    { model
                        | page = page
                        , content = BlogC blog
                    }
            in
                newModel ! [ Cmd.map BlogMsg blogCmd ]
