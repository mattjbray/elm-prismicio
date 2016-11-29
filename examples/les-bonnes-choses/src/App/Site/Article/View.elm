module App.Site.Article.View exposing (..)

import App.Site.Article.Types exposing (..)
import App.Documents.Types as Documents
import App.Common exposing (structuredTextAsHtml, viewLoading)
import Html exposing (..)
import Html.Attributes exposing (..)
import Prismic as P exposing (Url(Url))
import Result.Extra as Result


view : Model -> Html msg
view model =
    div [ class "main", id "about" ]
        (model.article
            |> Result.unpack viewError viewMArticle
        )


viewError : P.PrismicError -> List (Html msg)
viewError error =
    [ pre [] [ text (toString error) ] ]


viewMArticle : Maybe Documents.Article -> List (Html msg)
viewMArticle mArticle =
    case mArticle of
        Nothing ->
            [ viewLoading Nothing ]

        Just article ->
            viewArticle article


viewArticle : Documents.Article -> List (Html msg)
viewArticle article =
    let
        (Url imgUrl) =
            article.image.main.url
    in
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
