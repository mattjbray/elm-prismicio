module App.Blog.Common.View exposing (..)

import App.Types as App
import App.Blog.Types as Blog
import App.Documents.Types as Documents
import Html exposing (..)
import Html.Attributes exposing (class)
import App.Navigation exposing (toHash)


viewPostInfo : Documents.BlogPost -> Html msg
viewPostInfo blogPost =
    em [ class "infos" ]
        [ text (blogPost.date ++ " by " ++ blogPost.author) ]



blogPostUrl : Documents.BlogPost -> String
blogPostUrl blogPost =
  toHash (App.BlogP (Blog.PostP blogPost.id (Maybe.withDefault "post" (List.head blogPost.slugs))))
