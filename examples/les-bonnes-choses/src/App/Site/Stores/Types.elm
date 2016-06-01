module App.Site.Stores.Types exposing (..)

import App.Site.Stores.Index.Types as Index
import App.Site.Stores.Show.Types as Show


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
