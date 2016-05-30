module App.Site.Products.Index.View exposing (..)

import App.Navigation exposing (toHash)
import App.Types as App
import App.Site.Types as Site
import App.Site.Products.Index.Types exposing (..)
import App.Site.Products.Common.View exposing (toCurrency, viewProductShort)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, selected, style, src)


view : Model -> Html Msg
view model =
    div
        [ class "main"
        , id "products"
        ]
        [ section [ id "catalog" ]
            [ div [ class "products" ]
                [ ul []
                    (List.map viewProductShort (Maybe.withDefault [] model.products))
                ]
            , p []
                [ a [ href (toHash (App.SiteP (Site.AboutP))) ]
                    [ text "Close the products list" ]
                ]
            ]
        , pre [] [ text (Maybe.withDefault "" (Maybe.map toString model.error)) ]
        ]
