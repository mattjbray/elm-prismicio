module App.Blog.Index.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as Prismic


type alias Model =
    { docs : Maybe (List Documents.BlogPost)
    , prismic : Prismic.Cache Documents.BlogPost
    }


type Msg
    = SetResponse ( Prismic.Response Documents.BlogPost, Prismic.Cache Documents.BlogPost )
    | SetError Prismic.PrismicError
