module App.Site.Stores.Show.View exposing (..)

import App.Site.Stores.Show.Types exposing (..)
import App.Documents.Types as Documents
import App.Common exposing (structuredTextAsHtml, viewLoading, toCssUrl)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, selected, style, src)
import Result.Extra as Result
import Prismic.View exposing (getTexts)
import Prismic.Types as P
import String


view : Model -> Html Msg
view model =
    div
        [ class "main"
        , id "store"
        ]
        (model.store
            |> Result.mapBoth viewError
                viewMStore
        )


viewError : P.PrismicError -> List (Html msg)
viewError error =
    [ pre [] [ text (toString error) ] ]


viewMStore : Maybe Documents.Store -> List (Html msg)
viewMStore mStore =
    mStore
        |> Maybe.map viewStore
        |> Maybe.withDefault [ viewLoading ]


viewStore : Documents.Store -> List (Html msg)
viewStore store =
    [ section [ id "page-header" ]
        [ div [ style [ ( "background-image", toCssUrl store.image.main.url ) ] ]
            [ div []
                ([ h1 [] [ text (getTexts store.name) ] ]
                    ++ structuredTextAsHtml store.description
                )
            ]
        ]
    , section [ id "page-body" ]
        [ div
            [ id "map-canvas"
            , style
                [ ( "width", "400px" )
                , ( "height", "300px" )
                ]
            ]
            []
        , p [ class "address" ]
            ([ store.address
             , String.join " " [ store.city, store.zipcode ]
             , store.country
             ]
                |> List.map text
                |> List.intersperse (br [] [])
            )
        , aside []
            [ h4 [] [ text "Opening times" ]
            , dl []
                ([ ( .monday, "Monday" )
                 , ( .tuesday, "Tuesday" )
                 , ( .wednesday, "Wednesday" )
                 , ( .thursday, "Thursday" )
                 , ( .friday, "Friday" )
                 , ( .saturday, "Saturday" )
                 , ( .sunday, "Sunday" )
                 ]
                    |> List.map
                        (\( getHours, label ) ->
                            [ dt [] [ text label ]
                            , dd [] [ text (store |> getHours |> String.join " ") ]
                            ]
                        )
                    |> List.concat
                )
            ]
        ]
    ]
