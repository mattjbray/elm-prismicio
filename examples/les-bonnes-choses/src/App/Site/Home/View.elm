module App.Site.Home.View exposing (..)

import App.Documents.Types as Documents
import App.Navigation exposing (toHash)
import App.Site.Home.Types exposing (..)
import App.Site.Products.Common.View as Common
import App.Site.Products.Types as Products
import App.Site.Types as Site
import App.Types as App
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, href, id, rel, selected, style)
import Html.Events exposing (onClick)
import Result.Extra as Result


categories : List Documents.Category
categories =
    [ Documents.Macaron, Documents.Cupcake, Documents.Pie ]


view : Model -> Html Msg
view model =
    div [ class "main", id "home" ]
        [ viewCaroussel model
        ]


viewCaroussel : Model -> Html Msg
viewCaroussel model =
    section [ id "caroussel" ]
        [ nav []
            [ ul []
                (List.map
                    (\cat ->
                        li []
                            [ a
                                [ classList [ ( "selected", cat == model.category ) ]
                                , onClick (SetCategory cat)
                                ]
                                [ text (Common.categoryToString cat) ]
                            ]
                    )
                    categories
                )
            ]
        , div [ class "products" ]
            [ ul [ class "current" ]
                (model.products
                    |> Result.mapBoth (\err -> [ li [] [ pre [] [ text (toString err) ] ] ])
                        (List.map Common.viewProductShort << filterProducts model.category)
                )
            ]
        , p []
            [ a [ href (toHash (App.SiteP (Site.ProductsP (Products.IndexP Nothing)))) ]
                [ text "Browse all our products" ]
            ]
        ]


filterProducts : Documents.Category -> List Documents.Product -> List Documents.Product
filterProducts category products =
    products
        |> List.filter
            (\product ->
                List.member category product.categories
                    && not (List.member "Featured" product.tags)
            )
        |> List.take 5
