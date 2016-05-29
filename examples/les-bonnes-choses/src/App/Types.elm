module App.Types exposing (..)

import App.Blog.Types as Blog
import App.Site.Types as Site


type alias Model =
    { page : Page
    , content : Content
    }


type Page
    = SiteP Site.Page
    | BlogP Blog.Page


type Content
    = NoContent
    | BlogC Blog.Model
    | SiteC Site.Model


type Msg
    = SiteMsg Site.Msg
    | BlogMsg Blog.Msg
