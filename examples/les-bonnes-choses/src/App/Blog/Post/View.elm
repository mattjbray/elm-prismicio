module App.Blog.Post.View exposing (..)

import App.Blog.Post.Types exposing (..)
import App.Blog.Common.View exposing (viewPostInfo, blogPostUrl)
import App.Documents.Types as Documents
import Html exposing (..)
import Html.Attributes exposing (id, href)
import Prismic.View exposing (getText, getTitle, structuredTextAsHtml)


view : Model -> Html Msg
view model =
    case model.doc of
        Nothing ->
            p [] [ text "Blog Post is loading..." ]

        Just doc ->
            viewDocumentBlogPostFull doc model.relatedPosts


viewDocumentBlogPostFull : Documents.BlogPost -> List Documents.BlogPost -> Html Msg
viewDocumentBlogPostFull blogPost relatedPosts =
    let
        viewRelated post =
            let title =
                  case getTitle post.body of
                      Nothing ->
                        "No title"
                      Just heading ->
                        getText heading
            in
              li []
                  [ a
                      [href (blogPostUrl post)]
                      [text title]
                  ]
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
                , ul [] (List.map viewRelated relatedPosts)
                ]
            , aside []
                [ h2 [] [ text "Some pastries you should love" ]
                , ul [] (List.map viewLink blogPost.relatedProducts)
                ]
            ]
