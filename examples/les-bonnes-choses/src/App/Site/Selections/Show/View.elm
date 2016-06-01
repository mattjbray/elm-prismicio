module App.Site.Selections.Show.View exposing (..)

import App.Documents.Types as Documents
import App.Site.Selections.Show.Types exposing (..)
import App.Site.Products.Common.View as Common
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, selected, style, src)
import Prismic.Types as P exposing (Url(Url))
import Prismic.View exposing (getTexts, structuredTextAsHtml)
import Result.Extra as Result


view : Model -> Html Msg
view model =
    div
        [ class "main"
        , id "selection"
        ]
        (Result.mapBoth (\error -> [ viewError error ])
            viewMaybeSelection
            model.selection
            ++ [ section [ class "products" ]
                    [ h2 [] [ text "Part of this selection" ]
                    , Result.mapBoth viewError
                        (\products ->
                            ul [] (List.map Common.viewProductShort products)
                        )
                        model.products
                    ]
               ]
        )


viewError : P.PrismicError -> Html Msg
viewError error =
    pre [] [ text (toString error) ]


viewMaybeSelection : Maybe Documents.Selection -> List (Html Msg)
viewMaybeSelection mSelection =
    mSelection
        |> Maybe.map viewSelection
        |> Maybe.withDefault [ p [] [ text "No selection loaded." ] ]


toCssUrl : Url -> String
toCssUrl (Url url) =
    "url(" ++ url ++ ")"


viewSelection : Documents.Selection -> List (Html Msg)
viewSelection selection =
    [ viewPageHeader selection
    , viewPageBody selection
    ]


viewPageHeader : Documents.Selection -> Html Msg
viewPageHeader selection =
    section [ id "page-header" ]
        [ div [ style [ ( "background-image", toCssUrl selection.catcherImage.main.url ) ] ]
            [ div []
                [ h1 [] [ selection.name |> getTexts |> text ]
                , p [] [ selection.shortLede |> getTexts |> text ]
                ]
            ]
        ]


viewPageBody : Documents.Selection -> Html Msg
viewPageBody selection =
    let
        (Url imageUrl) =
            selection.image.main.url
    in
        section [ id "page-body" ]
            [ img [ src imageUrl ] []
            , div []
                (structuredTextAsHtml selection.description
                    ++ [ h4 []
                            [ em [] [ text (Common.toCurrency selection.price) ] ]
                       ]
                )
            ]
