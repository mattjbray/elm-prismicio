module App.Blog.Index.View exposing (..)

import App.Blog.Index.Types exposing (..)
import App.Documents.Types as Documents
import Prismic.View exposing (structuredTextAsHtml)
import Html exposing (..)


view : Model -> Html Msg
view model =
    case model.docs of
        Just docs ->
            div []
                (List.map viewDocumentBlogPostShort docs)

        Nothing ->
            p [] [ text "Blog index is loading..." ]


viewDocumentBlogPostShort : Documents.BlogPost -> Html Msg
viewDocumentBlogPostShort blogPost =
    div []
        [ a
            -- TODO: navigation
            -- [ onClick (NavigateTo (BlogPostP docId))
            -- , href (toHash (BlogPostP docId))
            []
            (structuredTextAsHtml blogPost.shortLede)
        , em []
            [ text ("Posted on " ++ blogPost.date ++ " by " ++ blogPost.author ++ " in " ++ blogPost.category) ]
        ]
