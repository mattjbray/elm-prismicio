module App.State exposing (..)

import App.Types exposing (..)
import App.Blog.State as Blog
import App.Navigation exposing (hashParser)
import App.Site.State as Site
import App.Site.Types as Site
import Prismic as P exposing (Url(Url))
import Navigation


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
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
        urlUpdate location model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model

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
                        | prismic =
                            P.collectResponses model.prismic prismic
                    }

                RenderNotFound ->
                    { mod
                        | content = NoContent
                        , page = NotFoundP
                    }
    in
        List.foldl processMsg model msgs


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case hashParser (Debug.log "location" location) of
        Nothing ->
            ( { model
                | page = NotFoundP
                , content = NoContent
              }
            , Cmd.none
            )

        Just NotFoundP ->
            ( { model
                | page = NotFoundP
                , content = NoContent
              }
            , Cmd.none
            )

        Just ((SiteP sitePage) as page) ->
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

        Just ((BlogP blogPage) as page) ->
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
