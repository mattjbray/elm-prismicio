module App.Blog.Index.View exposing (..)

import App.Blog.Index.Types exposing (..)
import App.Blog.Types exposing (Page(PostP))
import App.Documents.Types as Documents
import App.Navigation exposing (toHash)
import App.Types exposing (Page(BlogP))
import Html exposing (..)
import Html.Attributes exposing (href)
import Prismic.View exposing (structuredTextAsHtml)


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
        [ a [ href (toHash (BlogP (PostP blogPost.id))) ]
            (structuredTextAsHtml blogPost.shortLede)
        , em []
            [ text ("Posted on " ++ blogPost.date ++ " by " ++ blogPost.author ++ " in " ++ blogPost.category) ]
        ]
