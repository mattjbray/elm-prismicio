module App.Site.Search.Results.Types exposing (..)

import App.Documents.Types as Documents
import Dict exposing (Dict)
import Prismic as P


type alias Model =
    { products : Result P.PrismicError (List ProductR)
    , articles : Result P.PrismicError (List ArticleR)
    , bookmarks : Dict String String
    }


type Msg
    = SetProducts (Result P.PrismicError ( P.Response ProductR, P.Model ))
    | SetArticles (Result P.PrismicError ( P.Response ArticleR, P.Model ))


type ProductR
    = ProductR Documents.Product
    | SelectionR Documents.Selection


type ArticleR
    = ArticleR Documents.Article
    | BlogPostR Documents.BlogPost
    | StoreR Documents.Store
