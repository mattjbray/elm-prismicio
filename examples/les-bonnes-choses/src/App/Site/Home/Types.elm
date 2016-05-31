module App.Site.Home.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as P


type alias Model =
    { products : Result P.PrismicError (List Documents.Product)
    , featured : Result P.PrismicError (List Documents.Product)
    , category : Documents.Category
    }


type Msg
    = SetProducts (Result P.PrismicError ( P.Response Documents.Product, P.Cache ))
    | SetFeatured (Result P.PrismicError ( P.Response Documents.Product, P.Cache ))
    | SetCategory Documents.Category
