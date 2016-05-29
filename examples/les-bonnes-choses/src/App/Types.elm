module App.Types exposing (..)

import App.Article.Types as Article
import App.Blog.Types as Blog


type alias Model =
    { page : Page
    , content : Content
    }


type Page
    = SearchP String
    | BlogP Blog.Page
    | AboutP
    | JobsP
    | StoresP


type Content
    = NoContent
    | ArticleC Article.Model
    | BlogC Blog.Model


type Msg
    = ArticleMsg Article.Msg
    | BlogMsg Blog.Msg
