module App.View exposing (..)

import App.Types exposing (..)
import App.Site.View as Site
import App.Blog.View as Blog
import Html exposing (..)
import Html.App exposing (map)
import Html.Attributes exposing (class, classList, disabled, href, id, rel, selected, style)


view : Model -> Html Msg
view model =
    viewWithStyleSheet "http://lesbonneschoses.prismic.me/assets/stylesheets/normalize.min.css"
        [ viewContent model
        , viewFooter model
        ]


viewContent : Model -> Html Msg
viewContent model =
    case model.content of
        SiteC site ->
            viewWithStyleSheet "http://lesbonneschoses.prismic.me/assets/stylesheets/main.css"
                [map SiteMsg (Site.view site)]

        BlogC blog ->
            viewWithStyleSheet "http://lesbonneschoses.prismic.me/assets/stylesheets/blog.css"
                [map BlogMsg (Blog.view blog)]

        NoContent ->
            p [] [ text "No page loaded." ]


viewWithStyleSheet : String -> List (Html Msg) -> Html Msg
viewWithStyleSheet stylesheet elems =
    div []
        ([ node "link"
            [ rel "stylesheet", href stylesheet ]
            []
         ]
            ++ elems
        )


viewFooter : Model -> Html Msg
viewFooter _ =
    footer []
        [ text "This is a demonstration website for "
        , a [ href "https://github.com/mattjbray/elm-prismicio" ]
            [ text "elm-prismicio" ]
        ]
