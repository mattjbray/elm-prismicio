module App.Site.Products.Product.View exposing (..)

import App.Documents.Types as Documents
import App.Navigation exposing (toHash)
import App.Types as App
import App.Site.Types as Site
import App.Site.Products.Types as Products
import App.Site.Products.Product.Types exposing (..)
import App.Site.Products.Common.View exposing (toCurrency, viewProductShort)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, selected, style, src)
import Prismic.Types as P exposing (Url(Url))
import Prismic.View exposing (getTexts, structuredTextAsHtml)


view : Model -> Html Msg
view model =
    case model.product of
        Just product ->
            let
                (Url imageUrl) =
                    product.image.main.url
            in
                div
                    [ class "main"
                    , id "product"
                    ]
                    [ section [ id "detail" ]
                        [ div []
                            ([ img [ src imageUrl ] []
                             , h4 []
                                [ strong [] [ text (getTexts product.name) ]
                                , em [] [ text (toCurrency product.price) ]
                                ]
                             , h2 [] [ text (getTexts product.shortLede) ]
                             ]
                                ++ structuredTextAsHtml product.description
                                ++ [ p []
                                        [ a []
                                            [ strong
                                                [ class "color"
                                                , style [ ( "background", product.color ) ]
                                                ]
                                                [ text product.color ]
                                            ]
                                        ]
                                   ]
                            )
                        , p []
                            [ a [ href (toHash (App.SiteP (Site.ProductsP (Products.IndexP)))) ]
                                [ text "Browse all our products" ]
                            ]
                        ]
                    , case product.gallery of
                        image :: _ ->
                            viewGallery image

                        _ ->
                            viewTestamonial product
                    , viewRelatedProducts model
                    ]

        Nothing ->
            case model.error of
                Just error ->
                    pre [] [ text (toString error) ]

                Nothing ->
                    p [] [ text "Loading product..." ]


viewGallery : P.ImageField -> Html Msg
viewGallery image =
    let
        (Url url) =
            image.main.url
    in
        section
            [ id "gallery"
            , style [ ( "background-image", "url(" ++ url ++ ")" ) ]
            ]
            []


viewTestamonial : Documents.Product -> Html Msg
viewTestamonial product =
    let
        quoteSection author quote =
            section [ id "quote" ]
                [ blockquote []
                    [ text (getTexts quote)
                    , strong []
                        [ text "said "
                        , text (getTexts author)
                        ]
                    ]
                ]
    in
        Maybe.map2 quoteSection product.testimonialAuthor product.testimonialQuote
            |> Maybe.withDefault (text "")


viewRelatedProducts : Model -> Html Msg
viewRelatedProducts model =
  let viewRelated prod =
        li []
  in
    section
        [ id "related"
        , class "products"
        ]
        [ h2 [] [ text "You might like these as well" ]
        , ul [] (List.map viewProductShort model.relatedProducts)
        ]
