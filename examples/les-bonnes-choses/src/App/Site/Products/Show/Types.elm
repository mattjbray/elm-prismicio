module App.Site.Products.Show.Types exposing (..)

import App.Documents.Types as Documents
import Prismic as P


type alias Model =
    { product : Maybe Documents.Product
    , relatedProducts : List Documents.Product
    , error : Maybe P.PrismicError
    }


type Msg
    = SetResponse ( P.Response Documents.Product, P.Model )
    | SetRelatedProducts ( P.Response Documents.Product, P.Model )
    | SetError P.PrismicError
