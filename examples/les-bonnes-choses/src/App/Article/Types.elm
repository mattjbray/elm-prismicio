module App.Article.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as Prismic


type alias Model =
    { doc : Maybe Documents.Article
    , prismic : Prismic.Cache Documents.Article
    }


type Msg
    = SetResponse ( Prismic.Response Documents.Article, Prismic.Cache Documents.Article )
    | SetError Prismic.PrismicError
