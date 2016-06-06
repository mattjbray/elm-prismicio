module App.Site.Jobs.Show.Types exposing (..)

import App.Site.Article.Types as Article
import App.Documents.Types as Documents
import Prismic as P


type alias Model =
    { job : Result P.PrismicError (Maybe Documents.JobOffer)
    , article : Article.Model
    }


type Msg
    = SetJob (Result P.PrismicError ( P.Response Documents.JobOffer, P.Model ))
    | ArticleMsg Article.Msg
