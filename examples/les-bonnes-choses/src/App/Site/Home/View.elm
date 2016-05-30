module App.Site.Home.View exposing (..)

import App.Navigation exposing (toHash)
import App.Types as App
import App.Site.Types as Site
import App.Site.Products.Types as Products
import App.Site.Home.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, href, id, rel, selected, style)


view : Model -> Html Msg
view model =
    div [ class "main", id "home" ]
        [ section [ id "caroussel" ]
            [ nav []
                [ ul []
                    [ li [] [ a [] [ text "Macarons" ] ]
                    , li [] [ a [] [ text "Cup cakes" ] ]
                    , li [] [ a [] [ text "Little pies" ] ]
                    ]
                ]
            , div [ class "products" ] []
            , p []
                [ a [ href (toHash (App.SiteP (Site.ProductsP (Products.IndexP Nothing)))) ]
                    [ text "Browse all our products" ]
                ]
            ]
        ]
