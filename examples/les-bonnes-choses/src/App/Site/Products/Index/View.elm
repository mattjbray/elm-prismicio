module App.Site.Products.Index.View exposing (..)

import App.Navigation exposing (toHash)
import App.Types as App
import App.Site.Types as Site
import App.Site.Products.Types as Products
import App.Site.Products.Index.Types exposing (..)
import App.Site.Products.Common.View exposing (toCurrency)
import App.Documents.Types as Documents
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, selected, style, src)
import Prismic.View exposing (getTexts)
import Prismic.Types exposing (Url(Url))


view : Model -> Html Msg
view model =
    div
        [ class "main"
        , id "products"
        ]
        [ section
            [ id "catalog"
            , style [ ( "height", "1170px" ) ]
            ]
            [ div [ class "products" ]
                [ ul []
                    (List.map viewProduct (Maybe.withDefault [] model.products))
                ]
            , p []
                [ a [] [ text "Close the products list" ] ]
            ]
        , pre [] [ text (Maybe.withDefault "" (Maybe.map toString model.error)) ]
        ]


viewProduct : Documents.Product -> Html Msg
viewProduct product =
    let
        (Url imageUrl) =
            Dict.get "icon" product.image.views
                |> Maybe.map .url
                |> Maybe.withDefault (Url "")

        slug =
            product.slugs
                |> List.head
                |> Maybe.withDefault ""

        productUrl =
            (toHash (App.SiteP (Site.ProductsP (Products.ProductP product.id slug))))
    in
        li [ attribute "data-category" (Maybe.withDefault "" (List.head product.tags)) ]
            [ a [ href productUrl ]
                [ img [ src imageUrl ] []
                , span [] [ text (getTexts product.name) ]
                , em [] [ text (toCurrency product.price) ]
                ]
            ]
