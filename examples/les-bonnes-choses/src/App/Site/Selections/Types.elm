module App.Site.Selections.Types exposing (..)

import App.Site.Selections.Show.Types as Show


type alias Model =
    { page : Page
    , content : Content
    }


type Page
    = ShowP String String


type Content
    = NoContent
    | ShowC Show.Model


type Msg
    = ShowMsg Show.Msg
