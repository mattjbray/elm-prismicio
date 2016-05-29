module App.Site.Article.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as Prismic


type alias Model =
    { doc : Maybe Documents.Article
    , prismic : Prismic.Cache
    }


type Msg
    = SetResponse ( Prismic.Response Documents.Article, Prismic.Cache)
    | SetError Prismic.PrismicError
