module App.Site.Products.Types exposing (..)

import App.Site.Products.Index.Types as Index
import App.Site.Products.Show.Types as Show


type alias Model =
    { page : Page
    , content : Content
    }


type Page
    = IndexP (Maybe String)
    | ShowP String String


type Content
    = NoContent
    | IndexC Index.Model
    | ShowC Show.Model


type Msg
    = IndexMsg Index.Msg
    | ShowMsg Show.Msg
