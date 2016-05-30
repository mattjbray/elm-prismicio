module App.Site.Home.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as P


type alias Model =
    { doc : Maybe String
    }


type Msg
    = NoMsg
