module App.Site.Products.Product.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as P


type alias Model =
    { product : Maybe Documents.Product
    , error : Maybe P.PrismicError
    }


type Msg
    = SetResponse ( P.Response Documents.Product, P.Cache )
    | SetError P.PrismicError
