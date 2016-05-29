module App.Blog.View exposing (..)

import App.Blog.Types exposing (..)
import App.Blog.Index.View as Index
import App.Blog.Post.View as Post
import Html exposing (..)
import Html.App exposing (map)


view : Model -> Html Msg
view model =
    case model.content of
        IndexC index ->
            map IndexMsg (Index.view index)

        PostC post ->
            map PostMsg (Post.view post)

        NoContent ->
            p [] [ text "Blog is loading..." ]
