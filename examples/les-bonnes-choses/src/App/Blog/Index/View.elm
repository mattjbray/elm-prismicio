module App.Blog.Index.View exposing (..)

import App.Blog.Index.Types exposing (..)
import App.Blog.Common.View exposing (viewPostInfo, blogPostUrl)
import App.Documents.Types as Documents
import Html exposing (..)
import Html.Attributes exposing (class, id, href, style)
import Prismic.View exposing (getFirstImage, getFirstParagraph, getText, getTitle, structuredTextAsHtml)
import Prismic.Types exposing (Url(Url))


view : Model -> Html Msg
view model =
    case model.docs of
        Just docs ->
            section [ id "posts" ]
                (List.map viewDocumentBlogPostShort docs)

        Nothing ->
            p [] [ text "Blog index is loading..." ]


viewDocumentBlogPostShort : Documents.BlogPost -> Html Msg
viewDocumentBlogPostShort blogPost =
    let
        title =
            case getTitle blogPost.body of
                Nothing ->
                    "No Title"

                Just heading ->
                    getText heading

        firstPara =
            case getFirstParagraph blogPost.body of
                Nothing ->
                    "No text"

                Just paragraph ->
                    getText paragraph

        imageUrl =
            case getFirstImage blogPost.body of
                Just image ->
                    let
                        (Url url) =
                            image.url
                    in
                        url

                Nothing ->
                    ""
    in
        article []
            [ a [ href (blogPostUrl blogPost) ]
                [ viewPostInfo blogPost
                , h2 [] [ text title ]
                , p [] [ text firstPara ]
                , div
                    [ style
                        [ ( "background-image", "url(" ++ imageUrl ++ ")" )
                        , ( "background-size", "cover" )
                        ]
                    ]
                    []
                , strong [] [ text "Read more" ]
                ]
            ]
