module App.Site.Stores.Index.View exposing (..)

import App.Navigation exposing (urlForStore)
import App.Site.Stores.Index.Types exposing (..)
import App.Site.Article.View as Article
import App.Documents.Types as Documents
import App.Common exposing (structuredTextAsHtml, toCssUrl)
import Dict
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Prismic as P exposing (Url(Url))
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
                (List.map viewStore << List.sortBy (P.getTexts << .name))
        )


viewStore : Documents.Store -> Html msg
viewStore store =
    let
        imageUrl =
            store.image.views
                |> Dict.get "medium"
                |> Maybe.map .url
                |> Maybe.withDefault (Url "")
    in
        article
            [ class "store"
            , style [ ( "background-image", toCssUrl imageUrl ) ]
            ]
            [ a [ href (urlForStore store) ]
                [ h3 [] [ text (P.getTexts store.name) ] ]
            ]
