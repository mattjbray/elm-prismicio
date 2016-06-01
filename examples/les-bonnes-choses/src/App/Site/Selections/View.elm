module App.Site.Selections.View exposing (..)

import App.Site.Selections.Types exposing (..)
import App.Site.Selections.Show.View as Show
import Html exposing (..)
import Html.App exposing (map)


view : Model -> Html Msg
view model =
    case model.content of
        ShowC selection ->
            map ShowMsg (Show.view selection)

        NoContent ->
            p [] [ text "No selection content" ]
