module App.Site.Selections.Show.View exposing (..)

import App.Common exposing (structuredTextAsHtml, toCssUrl, viewError)
import App.Documents.Types as Documents
import App.Site.Selections.Show.Types exposing (..)
import App.Site.Products.Common.View as Common
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, selected, style, src)
import Prismic as P exposing (Url(Url))
import Result.Extra as Result


view : Model -> Html Msg
view model =
    div
        [ class "main"
        , id "selection"
        ]
        (Result.unpack (\error -> [ viewError error ])
            viewMaybeSelection
            model.selection
            ++ [ section [ class "products" ]
                    [ h2 [] [ text "Part of this selection" ]
                    , Result.unpack viewError
                        (\products ->
                            ul [] (List.map Common.viewProductShort products)
                        )
                        model.products
                    ]
               ]
        )


viewMaybeSelection : Maybe Documents.Selection -> List (Html Msg)
viewMaybeSelection mSelection =
    mSelection
        |> Maybe.map viewSelection
        |> Maybe.withDefault [ p [] [ text "No selection loaded." ] ]


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
                [ h1 [] [ selection.name |> P.getTexts |> text ]
                , p [] [ selection.shortLede |> P.getTexts |> text ]
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
