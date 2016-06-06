module App.Blog.Index.Types exposing (..)

import App.Documents.Types as Documents
import Prismic as P


type alias Model =
    { docs : Maybe (List Documents.BlogPost)
    }


type Msg
    = SetResponse ( P.Response Documents.BlogPost, P.Model )
    | SetError P.PrismicError
