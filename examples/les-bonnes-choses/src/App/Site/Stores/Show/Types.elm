module App.Site.Stores.Show.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as P


type alias Model =
    { store : Result P.PrismicError (Maybe Documents.Store)
    }


type Msg
    = SetStore (Result P.PrismicError ( P.Response Documents.Store, P.Cache ))
