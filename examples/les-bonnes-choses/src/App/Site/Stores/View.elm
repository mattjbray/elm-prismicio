module App.Site.Stores.View exposing (..)

import App.Site.Stores.Types exposing (..)
import App.Site.Stores.Index.View as Index
import App.Site.Stores.Show.View as Show
import Html exposing (..)
import Html.App exposing (map)


view : Model -> Html Msg
view model =
    case model.content of
        IndexC index ->
            map IndexMsg (Index.view index)

        ShowC store ->
            map ShowMsg (Show.view store)

        NoContent ->
            p [] [ text "No store content" ]
