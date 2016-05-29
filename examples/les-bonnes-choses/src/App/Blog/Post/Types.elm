module App.Blog.Post.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as P


type alias Model =
    { doc : Maybe Documents.BlogPost
    , relatedPosts : List Documents.BlogPost
    , relatedProducts : List Documents.Product
    , error : Maybe P.PrismicError
    }


type Msg
    = SetResponse ( P.Response Documents.BlogPost, P.Cache)
    | SetRelatedPosts ( P.Response Documents.BlogPost, P.Cache)
    | SetRelatedProducts ( P.Response Documents.Product, P.Cache)
    | SetError P.PrismicError
