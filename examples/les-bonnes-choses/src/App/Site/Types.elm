module App.Site.Types exposing (..)

import App.Site.Article.Types as Article
import App.Site.Home.Types as Home
import App.Site.Products.Types as Products
import App.Site.Selections.Types as Selections
import App.Site.Stores.Types as Stores


type alias Model =
    { page : Page
    , content : Content
    }


type Page
    = SearchP String
    | AboutP
    | JobsP
    | StoresP Stores.Page
    | ProductsP Products.Page
    | SelectionsP Selections.Page
    | HomeP


type Content
    = NoContent
    | ArticleC Article.Model
    | ProductsC Products.Model
    | SelectionsC Selections.Model
    | StoresC Stores.Model
    | HomeC Home.Model


type Msg
    = ArticleMsg Article.Msg
    | ProductsMsg Products.Msg
    | SelectionsMsg Selections.Msg
    | StoresMsg Stores.Msg
    | HomeMsg Home.Msg
