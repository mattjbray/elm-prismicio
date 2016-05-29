module App.Site.Products.Types exposing (..)

import App.Documents.Types as Documents
import App.Site.Products.Index.Types as Index


type alias Model =
    { page : Page
    , content : Content
    }

type Page
  = IndexP


type Content
  = NoContent
  | IndexC Index.Model


type Msg
    = IndexMsg Index.Msg
