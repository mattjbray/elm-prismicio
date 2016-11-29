module App.View exposing (..)

import App.Types exposing (..)
import App.Site.View as Site
import App.Types as App
import App.Navigation exposing (urlForHome)
import App.Blog.View as Blog
import App.Common as Common
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, href, id, rel, selected, style)


view : Model -> Html Msg
view model =
    viewWithStyleSheet "assets/css/normalize.min.css"
        [ viewContent model
        , viewFooter model
        ]


viewContent : Model -> Html Msg
viewContent model =
    case model.content of
        SiteC site ->
            viewWithStyleSheet "assets/css/main.css"
                [ map SiteMsg (Site.view site) ]

        BlogC blog ->
            viewWithStyleSheet "assets/css/blog.css"
                [ map BlogMsg (Blog.view blog) ]

        NoContent ->
            viewWithStyleSheet "assets/css/main.css"
                viewNotFound


viewWithStyleSheet : String -> List (Html Msg) -> Html Msg
viewWithStyleSheet stylesheet elems =
    div []
        ([ node "link"
            [ rel "stylesheet", href stylesheet ]
            []
         ]
            ++ elems
        )


viewNotFound : List (Html msg)
viewNotFound =
    [ Common.viewHeader NotFoundP
    , div [ class "main", id "not-found" ]
        [ section []
            [ h1 [] [ text "Page not found" ] ]
        , section []
            [ p [] [ text "We can't seem to find what you are looking for." ]
            , p []
                [ a [ href urlForHome ]
                    [ text "Go to the home page" ]
                ]
            ]
        ]
    ]


viewFooter : Model -> Html Msg
viewFooter _ =
    footer []
        [ text "This is a demonstration website for "
        , a [ href "https://github.com/mattjbray/elm-prismicio" ]
            [ text "elm-prismicio" ]
        ]
