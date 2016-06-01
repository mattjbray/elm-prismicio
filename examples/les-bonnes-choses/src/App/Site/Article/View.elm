module App.Site.Article.View exposing (..)

import App.Site.Article.Types exposing (..)
import App.Common exposing (structuredTextAsHtml)
import Html exposing (..)
import Html.Attributes exposing (..)
import Prismic.Types exposing (Url(Url))


view : Model -> Html msg
view model =
    case model.doc of
        Nothing ->
            p [] [ text "Article is loading..." ]

        Just article ->
            let
                (Url imgUrl) =
                    article.image.main.url
            in
                div [ class "main", id "about" ]
                    [ section [ id "page-header" ]
                        [ div [ style [ ( "background-image", "url(" ++ imgUrl ++ ")" ) ] ]
                            [ div []
                                (structuredTextAsHtml article.title
                                    ++ structuredTextAsHtml article.shortLede
                                )
                            ]
                        ]
                    , section [ id "page-body" ]
                        (structuredTextAsHtml article.content)
                    ]
