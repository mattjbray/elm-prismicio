module App.Site.Search.View exposing (..)

import App.Site.Search.Types exposing (..)
import App.Site.Search.Results.View as Results
import Html exposing (..)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)


view : Model -> Html Msg
view model =
    div [ class "main", id "search" ]
        [ section []
            [ form [ onSubmit Submit ]
                [ input
                    [ type_ "text"
                    , placeholder "Search anything"
                    , value model.query
                    , onInput SetQuery
                    ]
                    []
                , input [ type_ "submit", value "submit" ] []
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
