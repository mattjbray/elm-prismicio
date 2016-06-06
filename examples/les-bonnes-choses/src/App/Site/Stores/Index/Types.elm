module App.Site.Stores.Index.Types exposing (..)

import App.Site.Article.Types as Article
import App.Documents.Types as Documents
import Prismic as P


type alias Model =
    { article : Article.Model
    , stores : Result P.PrismicError (List Documents.Store)
    }


type Msg
    = ArticleMsg Article.Msg
    | SetStores (Result P.PrismicError ( P.Response Documents.Store, P.Model ))
