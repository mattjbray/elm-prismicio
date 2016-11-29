module App.Site.Home.Types exposing (..)

import App.Documents.Types as Documents
import Prismic as P


type Featured
    = ProductF Documents.Product
    | BlogPostF Documents.BlogPost
    | SelectionF Documents.Selection


type alias Model =
    { products : Result P.PrismicError (List Documents.Product)
    , featured : Result P.PrismicError (List Featured)
    , category : Documents.Category
    }


type Msg
    = FetchData (Result P.PrismicError P.ModelWithApi)
    | SetProducts (Result P.PrismicError ( P.Response Documents.Product, P.Model ))
    | SetFeatured (Result P.PrismicError ( P.Response Featured, P.Model ))
    | SetCategory Documents.Category
