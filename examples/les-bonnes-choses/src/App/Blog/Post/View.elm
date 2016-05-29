module App.Blog.Post.View exposing (..)

import App.Blog.Post.Types exposing (..)
import App.Documents.Types as Documents
import Html exposing (..)
import Prismic.View exposing (structuredTextAsHtml)


view : Model -> Html Msg
view model =
    case model.doc of
        Nothing ->
            p [] [ text "Blog Post is loading..." ]

        Just doc ->
            viewDocumentBlogPostFull doc


viewDocumentBlogPostFull : Documents.BlogPost -> Html Msg
viewDocumentBlogPostFull blogPost =
    div []
        ([ p [] [ text "BlogPost" ] ]
            ++ (structuredTextAsHtml blogPost.body)
            ++ [ em [] [ text ("Posted on " ++ blogPost.date ++ " by " ++ blogPost.author ++ " in " ++ blogPost.category) ]
               , p []
                    [ text
                        ("Comments are "
                            ++ (if blogPost.allowComments then
                                    "enabled"
                                else
                                    "disabled"
                               )
                            ++ "."
                        )
                    ]
               ]
        )
