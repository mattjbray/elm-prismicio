module App.Blog.Index.View exposing (..)

import App.Blog.Index.Types exposing (..)
import App.Blog.Common.View exposing (viewPostInfo)
import App.Navigation exposing (urlForBlogPost)
import App.Documents.Types as Documents
import Html exposing (..)
import Html.Attributes exposing (class, id, href, style)
import Prismic as P exposing (Url(Url))


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
            case P.getTitle blogPost.body of
                Nothing ->
                    "No Title"

                Just heading ->
                    P.getText heading

        firstPara =
            case P.getFirstParagraph blogPost.body of
                Nothing ->
                    "No text"

                Just paragraph ->
                    P.getText paragraph

        imageUrl =
            case P.getFirstImage blogPost.body of
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
            [ a [ href (urlForBlogPost blogPost) ]
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
