module App.Site.Search.View exposing (..)

import App.Site.Search.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, id, placeholder, type', value)


view : Model -> Html Msg
view model =
    div [ class "main", id "search" ]
        [ section []
            [ form []
                [ input
                    [ type' "text"
                    , placeholder "Search anything"
                    ]
                    []
                , input [ type' "submit", value "submit" ] []
                ]
            ]
        , section [] []
        ]
