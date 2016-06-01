module App.Site.Products.View exposing (..)

import App.Site.Products.Types exposing (..)
import App.Site.Products.Index.View as Index
import App.Site.Products.Show.View as Show
import Html exposing (..)
import Html.App exposing (map)


view : Model -> Html Msg
view model =
    case model.content of
        IndexC index ->
            map IndexMsg (Index.view index)

        ShowC product ->
            map ShowMsg (Show.view product)

        NoContent ->
            p [] [ text "No Products content" ]
