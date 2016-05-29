module App.Blog.Post.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as Prismic


type alias Model =
    { doc : Maybe Documents.BlogPost
    , relatedPosts : List Documents.BlogPost
    , prismic : Prismic.Cache
    }


type Msg
    = SetResponse ( Prismic.Response Documents.BlogPost, Prismic.Cache)
    | SetRelatedPosts ( Prismic.Response Documents.BlogPost, Prismic.Cache)
    | SetError Prismic.PrismicError
