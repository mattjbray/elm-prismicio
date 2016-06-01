module App.Site.Stores.Index.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as P


type alias Model =
    { article : Result P.PrismicError (Maybe Documents.Article)
    , stores : Result P.PrismicError (List Documents.Store)
    }


type Msg
    = SetArticle (Result P.PrismicError ( P.Response Documents.Article, P.Cache ))
    | SetStores (Result P.PrismicError ( P.Response Documents.Store, P.Cache ))
