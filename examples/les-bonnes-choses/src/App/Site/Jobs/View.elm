module App.Site.Jobs.View exposing (..)

import App.Site.Jobs.Types exposing (..)
import App.Site.Jobs.Index.View as Index
import App.Site.Jobs.Show.View as Show
import Html exposing (..)


view : Model -> Html Msg
view model =
    case model.content of
        IndexC index ->
            map IndexMsg (Index.view index)

        ShowC job ->
            map ShowMsg (Show.view job)

        NoContent ->
            p [] [ text "No job content" ]
