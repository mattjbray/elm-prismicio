module App.Types exposing (..)

import App.Blog.Types as Blog
import App.Site.Types as Site
import Prismic.Types as P


type alias Model =
    { page : Page
    , content : Content
    , prismic : P.Cache
    }


type Page
    = SiteP Site.Page
    | BlogP Blog.Page
    | NotFoundP


type Content
    = NoContent
    | BlogC Blog.Model
    | SiteC Site.Model


type Msg
    = SiteMsg Site.Msg
    | BlogMsg Blog.Msg


type GlobalMsg
    = SetPrismic P.Cache
