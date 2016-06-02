module App.Site.Products.Common.View exposing (..)

import App.Navigation exposing (urlForProduct)
import App.Documents.Types as Documents
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, selected, style, src)
import Prismic.Types exposing (Url(Url))
import Prismic.View exposing (getTexts)
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


categoryToString : Documents.Category -> String
categoryToString category =
    case category of
        Documents.Macaron ->
            "Macarons"

        Documents.Cupcake ->
            "Cupcakes"

        Documents.Pie ->
            "Pies"


viewProductShort : Documents.Product -> Html msg
viewProductShort product =
    let
        (Url imageUrl) =
            Dict.get "icon" product.image.views
                |> Maybe.map .url
                |> Maybe.withDefault (Url "")
    in
        li [ attribute "data-category" (Maybe.withDefault "" (Maybe.map categoryToString (List.head product.categories))) ]
            [ a [ href (urlForProduct product) ]
                [ img [ src imageUrl ] []
                , span [] [ text (getTexts product.name) ]
                , em [] [ text (toCurrency product.price) ]
                ]
            ]


viewFeaturedProduct : Documents.Product -> Html msg
viewFeaturedProduct product =
    let
        (Url backgroundImgUrl) =
            (List.head product.gallery
                |> Maybe.map .views
            )
                `Maybe.andThen` Dict.get "squared"
                |> Maybe.map .url
                |> Maybe.withDefault (Url "")
    in
        div [ style [ ( "background-image", "url(" ++ backgroundImgUrl ++ ")" ) ] ]
            [ a [ href (urlForProduct product) ]
                [ h3 [] [ span [] [ text (getTexts product.name) ] ]
                , p [] [ span [] [ text (getTexts product.shortLede) ] ]
                ]
            ]
