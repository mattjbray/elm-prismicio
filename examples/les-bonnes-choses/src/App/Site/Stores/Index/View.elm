module App.Site.Stores.Index.View exposing (..)

import App.Navigation exposing (toHash)
import App.Site.Stores.Index.Types exposing (..)
import App.Types as App
import App.Site.Types as Site
import App.Site.Stores.Types as Stores
import App.Site.Article.View as Article
import App.Documents.Types as Documents
import App.Common exposing (structuredTextAsHtml, toCssUrl)
import Dict
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Prismic.Types as P exposing (Url(Url))
import Prismic.View exposing (getTexts)
import Result.Extra as Result


view : Model -> Html Msg
view model =
    div [ class "main", id "stores" ]
        [ Html.map ArticleMsg (Article.view model.article)
        , viewStores model
        ]


viewError : P.PrismicError -> List (Html msg)
viewError error =
    [ pre [] [ text (toString error) ] ]


viewStores : Model -> Html msg
viewStores model =
    section [ id "page-body"
            , style [("margin-top", "-120px")]
            ]
        (model.stores
            |> Result.mapBoth viewError
                (List.map viewStore << List.sortBy (getTexts << .name))
        )


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
