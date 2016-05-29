module App.State exposing (..)

import App.Types exposing (..)
import App.Navigation exposing (toHash)
import App.Blog.State as Blog
import App.Site.State as Site
import App.Site.Types as Site
import Navigation
import Prismic.Types as P exposing (Url(Url))
import Prismic.State as P


init : Result String Page -> ( Model, Cmd Msg )
init result =
    let
        model =
            { page =
                SiteP Site.AboutP
            , content =
                NoContent
            , prismic =
                P.initCache (Url "https://lesbonneschoses.prismic.io/api")
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
                        ( newSite, siteCmd, mNewPrismic ) =
                            Site.update siteMsg site
                    in
                        ( { model
                            | content = SiteC newSite
                            , prismic = Maybe.withDefault model.prismic mNewPrismic
                          }
                        , Cmd.map SiteMsg siteCmd
                        )

                _ ->
                    model ! []

        BlogMsg blogMsg ->
            case model.content of
                BlogC blog ->
                    let
                        ( newBlog, blogCmd, mNewPrismic ) =
                            Blog.update blogMsg blog
                    in
                        ( { model
                            | content = BlogC newBlog
                            , prismic = Maybe.withDefault model.prismic mNewPrismic
                          }
                        , Cmd.map BlogMsg blogCmd
                        )

                _ ->
                    model ! []


urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case Debug.log "result" result of
        Err _ ->
            ( model, Navigation.modifyUrl (toHash model.page) )

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
