module App.Site.Products.View exposing (..)

import App.Site.Products.Types exposing (..)
import App.Site.Products.Index.View as Index
import Html exposing (..)
import Html.App exposing (map)


view : Model -> Html Msg
view model =
  case model.content of
    IndexC index ->
      map IndexMsg (Index.view index)

    NoContent ->
      p [] [text "No Products content"]
