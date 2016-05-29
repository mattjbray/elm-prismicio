module App.Site.Products.Common.View exposing (..)

import App.Navigation exposing (toHash)
import App.Types as App
import App.Site.Types as Site
import App.Site.Products.Types as Products
import App.Documents.Types as Documents
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, selected, style, src)
import Prismic.View exposing (getTexts)
import Prismic.Types exposing (Url(Url))
import String


toCurrency : Float -> String
toCurrency amount =
    let
        amountStr =
            toString amount

        amountStr' =
            if String.contains "." amountStr then
                amountStr
            else
                (amountStr ++ ".")

        parts =
            String.split "." amountStr'

        build strs =
            case strs of
                [ str ] ->
                    String.padRight 2 '0' str :: build []

                str :: rest ->
                    str :: build rest

                [] ->
                    []
    in
        "$" ++ String.join "." (build parts)


viewProductShort : Documents.Product -> Html msg
viewProductShort product =
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
