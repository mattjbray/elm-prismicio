module Pages.Homepage exposing (..)

import Documents.Homepage exposing (Homepage)
import Documents.Menu exposing (Menu)
import Html exposing (Html)
import Html.Attributes as Html
import Pages.Views exposing (linkAttrs, viewBodySlice, viewHeader)
import Prismic.Document as Prismic
import Prismic.Url exposing (Url(Url))


view : Menu -> Homepage -> Html Prismic.DocumentReference
view menu homepage =
    Html.div [ Html.class "homepage" ]
        [ viewHeader menu
        , viewBanner homepage
        , Html.div [ Html.class "container" ]
            (List.map viewBodySlice homepage.body)
        ]


viewBanner : Homepage -> Html Prismic.DocumentReference
viewBanner homepage =
    let
        (Url imgSrc) =
            homepage.backgroundImage.main.url
    in
    Html.section
        [ Html.class "homepage-banner"
        , Html.attribute "style"
            ([ "background-image: linear-gradient(rgba(0, 0, 0, 0.4)"
             , "rgba(0, 0, 0, 0.6))"
             , "url(" ++ imgSrc ++ ")"
             ]
                |> String.join ", "
            )
        ]
        [ Html.div [ Html.class "banner-content container" ]
            [ Html.h2 [ Html.class "banner-title" ]
                [ Html.text (Prismic.getTexts homepage.title) ]
            , Html.p [ Html.class "banner-description" ]
                [ Html.text (Prismic.getTexts homepage.tagline) ]
            , Html.a (Html.class "banner-button" :: linkAttrs homepage.buttonLink)
                [ Html.text homepage.buttonText ]
            ]
        ]
