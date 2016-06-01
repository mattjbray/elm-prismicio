module App.Site.Stores.Index.View exposing (..)

import App.Navigation exposing (toHash)
import App.Site.Stores.Index.Types exposing (..)
import App.Types as App
import App.Site.Types as Site
import App.Site.Stores.Types as Stores
import App.Documents.Types as Documents
import App.Common exposing (structuredTextAsHtml, toCssUrl)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Prismic.Types as P exposing (Url(Url))
import Prismic.View exposing (getTexts)
import Result.Extra as Result


view : Model -> Html msg
view model =
    div [ class "main", id "stores" ]
        (model.article
            |> Result.mapBoth viewError
                (viewMArticle model)
        )


viewError : P.PrismicError -> List (Html msg)
viewError error =
    [ pre [] [ text (toString error) ] ]


viewMArticle : Model -> Maybe Documents.Article -> List (Html msg)
viewMArticle model mArticle =
    case mArticle of
        Nothing ->
            [ viewLoading ]

        Just article ->
            viewArticle model article


viewLoading : Html msg
viewLoading =
    section [ id "page-header" ]
        [ div []
            [ div []
                [ h1 [] [ text "Loading article..." ]
                ]
            ]
        ]


viewArticle : Model -> Documents.Article -> List (Html msg)
viewArticle model article =
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
            (structuredTextAsHtml article.content
                ++ viewStores model
            )
        ]


viewStores : Model -> List (Html msg)
viewStores model =
    model.stores
        |> Result.mapBoth viewError
            (List.map viewStore << List.sortBy (getTexts << .name))


viewStore : Documents.Store -> Html msg
viewStore store =
    let
        imageUrl =
            store.image.views
                |> Dict.get "medium"
                |> Maybe.map .url
                |> Maybe.withDefault (Url "")

        slug =
            store.slugs
                |> List.head
                |> Maybe.withDefault ""
    in
        article
            [ class "store"
            , style [ ( "background-image", toCssUrl imageUrl ) ]
            ]
            [ a [ href (toHash <| App.SiteP <| Site.StoresP <| Stores.ShowP store.id slug) ]
                [ h3 [] [ text (getTexts store.name) ] ]
            ]
