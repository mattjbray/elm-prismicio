module App.Site.Types exposing (..)

import App.Site.Article.Types as Article
import App.Site.Home.Types as Home
import App.Site.Jobs.Types as Jobs
import App.Site.Products.Types as Products
import App.Site.Search.Types as Search
import App.Site.Selections.Types as Selections
import App.Site.Stores.Types as Stores


type alias Model =
    { page : Page
    , content : Content
    }


type Page
    = AboutP
    | JobsP Jobs.Page
    | SearchP Search.Page
    | StoresP Stores.Page
    | ProductsP Products.Page
    | SelectionsP Selections.Page
    | HomeP


type Content
    = NoContent
    | ArticleC Article.Model
    | JobsC Jobs.Model
    | ProductsC Products.Model
    | SearchC Search.Model
    | SelectionsC Selections.Model
    | StoresC Stores.Model
    | HomeC Home.Model


type Msg
    = ArticleMsg Article.Msg
    | JobsMsg Jobs.Msg
    | ProductsMsg Products.Msg
    | SearchMsg Search.Msg
    | SelectionsMsg Selections.Msg
    | StoresMsg Stores.Msg
    | HomeMsg Home.Msg
