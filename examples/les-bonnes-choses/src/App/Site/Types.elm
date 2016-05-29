module App.Site.Types exposing (..)

import App.Site.Article.Types as Article


type alias Model =
    { page : Page
    , content : Content
    }


type Page
    = SearchP String
    | AboutP
    | JobsP
    | StoresP


type Content
    = NoContent
    | ArticleC Article.Model


type Msg
    = ArticleMsg Article.Msg
