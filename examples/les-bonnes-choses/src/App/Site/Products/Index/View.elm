module App.Site.Products.Index.View exposing (..)

import App.Site.Products.Index.Types exposing (..)
import App.Documents.Types as Documents
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, selected, style, src)
import Prismic.View exposing (getTexts)
import Prismic.Types exposing (Url(Url))
import String


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


viewProduct : Documents.Product -> Html Msg
viewProduct product =
    let
        (Url imageUrl) =
            Dict.get "icon" product.image.views
                |> Maybe.map .url
                |> Maybe.withDefault (Url "")
    in
        li [ attribute "data-category" (Maybe.withDefault "" (List.head product.tags)) ]
            [ a []
                [ img [ src imageUrl ] []
                , span [] [ text (getTexts product.name) ]
                , em [] [ text (toCurrency product.price) ]
                ]
            ]
