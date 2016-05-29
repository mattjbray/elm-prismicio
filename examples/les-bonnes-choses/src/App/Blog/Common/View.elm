module App.Blog.Common.View exposing (..)

import App.Documents.Types as Documents
import Html exposing (..)
import Html.Attributes exposing (class)


viewPostInfo : Documents.BlogPost -> Html msg
viewPostInfo blogPost =
    em [ class "infos" ]
        [ text (blogPost.date ++ " by " ++ blogPost.author) ]
