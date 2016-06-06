module App.Site.Jobs.Types exposing (..)

import App.Site.Jobs.Index.Types as Index
import App.Site.Jobs.Show.Types as Show


type alias Model =
    { page : Page
    , content : Content
    }


type Page
    = IndexP
    | ShowP String String


type Content
    = NoContent
    | IndexC Index.Model
    | ShowC Show.Model


type Msg
    = IndexMsg Index.Msg
    | ShowMsg Show.Msg
