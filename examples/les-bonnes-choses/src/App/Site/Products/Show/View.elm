module App.Site.Products.Show.View exposing (..)

import App.Common exposing (structuredTextAsHtml)
import App.Documents.Types as Documents
import App.Navigation exposing (urlForProducts, urlForProductsByFlavour)
import App.Site.Products.Common.View exposing (toCurrency, viewProductShort)
import App.Site.Products.Show.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, selected, style, src)
import Prismic.Types as P exposing (Url(Url))
import Prismic.View exposing (getTexts)


view : Model -> Html Msg
view model =
    case model.product of
        Just product ->
            let
                (Url imageUrl) =
                    product.image.main.url

                primaryFlavour =
                    List.head product.flavours
                        |> Maybe.withDefault ""
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
                                        [ a [ href (urlForProductsByFlavour primaryFlavour) ]
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
                            [ a [ href urlForProducts ]
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
    if List.isEmpty model.relatedProducts then
        text ""
    else
        section
            [ id "related"
            , class "products"
            ]
            [ h2 [] [ text "You might like these as well" ]
            , ul [] (List.map viewProductShort model.relatedProducts)
            ]
