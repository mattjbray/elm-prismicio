module App.Blog.View exposing (..)

import App.Blog.Index.View as Index
import App.Blog.Post.View as Post
import App.Blog.Types exposing (..)
import App.Navigation exposing (urlForHome, urlForBlog, urlForBlogCategory)
import Html exposing (..)
import Html.App exposing (map)
import Html.Attributes exposing (rel, href, class)


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , viewContent model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    header []
        [ a [ href urlForHome ]
            [ h1 [] [ text "Les Bonnes Choses" ] ]
        , nav []
            [ ul []
                [ li []
                    [ a [ href urlForBlog ]
                        [ text "Home" ]
                    ]
                , li []
                    [ a [ href (urlForBlogCategory "Announcements") ]
                        [ text "Announcements" ]
                    ]
                , li []
                    [ a [ href (urlForBlogCategory "Do it yourself") ]
                        [ text "Do it yourself" ]
                    ]
                , li []
                    [ a [ href (urlForBlogCategory "Behind the scenes") ]
                        [ text "Behind the scenes" ]
                    ]
                ]
            ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    div [ class "main" ]
        [ case model.content of
            IndexC index ->
                map IndexMsg (Index.view index)

            PostC post ->
                map PostMsg (Post.view post)

            NoContent ->
                p [] [ text "Blog is loading..." ]
        ]
