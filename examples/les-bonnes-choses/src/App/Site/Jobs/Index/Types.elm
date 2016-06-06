module App.Site.Jobs.Index.Types exposing (..)

import App.Site.Article.Types as Article
import App.Documents.Types as Documents
import Prismic as P


type alias Model =
    { article : Article.Model
    , jobs : Result P.PrismicError (List Documents.JobOffer)
    }


type Msg
    = ArticleMsg Article.Msg
    | SetJobs (Result P.PrismicError ( P.Response Documents.JobOffer, P.Model ))
