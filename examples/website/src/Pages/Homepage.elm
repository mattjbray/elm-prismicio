module Pages.Homepage exposing (..)

import Documents.Homepage exposing (Homepage)
import Documents.Menu exposing (Menu)
import Html exposing (Html)
import Html.Attributes as Html
import Pages.Views exposing (viewBodySlice, viewHeader)
import Prismic.Document.Field as Prismic


view : Prismic.LinkResolver msg -> Menu -> Homepage -> Html msg
view linkResolver menu homepage =
    Html.div [ Html.class "homepage" ]
        [ viewHeader linkResolver menu
        , viewBanner linkResolver homepage
        , Html.div [ Html.class "container" ]
            (List.map viewBodySlice homepage.body)
        ]


viewBanner : Prismic.LinkResolver msg -> Homepage -> Html msg
viewBanner linkResolver homepage =
    Html.section
        [ Html.class "homepage-banner"
        , Html.attribute "style"
            ([ "background-image: linear-gradient(rgba(0, 0, 0, 0.4)"
             , "rgba(0, 0, 0, 0.6))"
             , "url(" ++ homepage.backgroundImage.main.url ++ ")"
             ]
                |> String.join ", "
            )
        ]
        [ Html.div [ Html.class "banner-content container" ]
            [ Html.h2 [ Html.class "banner-title" ]
                [ Html.text (Prismic.getTexts homepage.title) ]
            , Html.p [ Html.class "banner-description" ]
                [ Html.text (Prismic.getTexts homepage.tagline) ]
            , Html.a (Html.class "banner-button" :: Prismic.resolveLink linkResolver homepage.buttonLink)
                [ Html.text homepage.buttonText ]
            ]
        ]
