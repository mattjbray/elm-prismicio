module App.Site.Article.Types exposing (..)

import App.Documents.Types as Documents
import Prismic as P


type alias Model =
    { article : Result P.PrismicError (Maybe Documents.Article)
    }


type Msg
    = SetArticle (Result P.PrismicError ( P.Response Documents.Article, P.Model ))
