module App.Blog.Post.View exposing (..)

import App.Blog.Post.Types exposing (..)
import App.Blog.Common.View exposing (viewPostInfo)
import App.Documents.Types as Documents
import Html exposing (..)
import Html.Attributes exposing (id)
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
    let
        viewLink link =
            li []
                [ text (toString link) ]
    in
        div []
            [ section [ id "post" ]
                [ viewPostInfo blogPost
                , article []
                    (structuredTextAsHtml blogPost.body)
                , h2 [] [ text "These should interest you too" ]
                , ul [] (List.map viewLink blogPost.relatedPosts)
                ]
            , aside []
                [ h2 [] [ text "Some pastries you should love" ]
                , ul [] (List.map viewLink blogPost.relatedProducts)
                ]
            ]
