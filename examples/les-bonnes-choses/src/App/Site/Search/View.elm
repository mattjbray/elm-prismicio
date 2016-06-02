module App.Site.Search.View exposing (..)

import App.Site.Search.Types exposing (..)
import App.Site.Search.Results.View as Results
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class, id, placeholder, type', value)
import Html.Events exposing (onInput, onSubmit)


view : Model -> Html Msg
view model =
    div [ class "main", id "search" ]
        [ section []
            [ form [ onSubmit Submit ]
                [ input
                    [ type' "text"
                    , placeholder "Search anything"
                    , value model.query
                    , onInput SetQuery
                    ]
                    []
                , input [ type' "submit", value "submit" ] []
                ]
            ]
        , section []
            [ case model.content of
                ResultsC results ->
                    Html.map ResultsMsg (Results.view results)
                _ ->
                  text ""
            ]
        ]
