module App.Site.Article.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as P


type alias Model =
    { doc : Maybe Documents.Article
    }


type Msg
    = SetResponse ( P.Response Documents.Article, P.Cache )
    | SetError P.PrismicError
