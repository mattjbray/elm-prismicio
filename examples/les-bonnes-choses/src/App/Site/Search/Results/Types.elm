module App.Site.Search.Results.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as P


type alias Model =
    { products : Result P.PrismicError (List ProductR)
    , articles : Result P.PrismicError (List ArticleR)
    }


type Msg
    = SetProducts (Result P.PrismicError ( P.Response ProductR, P.Cache ))
    | SetArticles (Result P.PrismicError ( P.Response ArticleR, P.Cache ))


type ProductR
    = ProductR Documents.Product
    | SelectionR Documents.Selection


type ArticleR
    = ArticleR Documents.Article
    | BlogPostR Documents.BlogPost
    | StoreR Documents.Store
