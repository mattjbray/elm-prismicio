module App.Site.Products.Index.Types exposing (..)

import App.Documents.Types as Documents
import Prismic as P


type alias Model =
    { products : Maybe (List Documents.Product)
    , error : Maybe P.PrismicError
    }


type Msg
    = SetResponse ( P.Response Documents.Product, P.Model )
    | SetError P.PrismicError
