module App.Blog.View exposing (..)

import App.Blog.Types exposing (..)
import App.Blog.Index.View as Index
import App.Blog.Post.View as Post
import App.Navigation exposing (toHash)
import App.Types as App
import App.Site.Types as Site
import Html exposing (..)
import Html.App exposing (map)
import Html.Attributes exposing (rel, href, class)


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , viewContent model
          --, viewFooter model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    header []
        [ a [ href (toHash (App.SiteP Site.AboutP)) ]
            [ h1 [] [ text "Les Bonnes Choses" ] ]
        ]


viewContent : Model -> Html Msg
viewContent model =
  div [class "main"]
    [case model.content of
        IndexC index ->
            map IndexMsg (Index.view index)

        PostC post ->
            map PostMsg (Post.view post)

        NoContent ->
            p [] [ text "Blog is loading..." ]
    ]

