module App.Types exposing (..)

import App.Article.Types as Article
import App.Blog.Types as Blog
import App.Documents.Types as Documents
import Prismic.Types exposing (PrismicError, Response, Api, StructuredText, Link, DefaultDocType, ImageField)


type alias Model =
    { response : Maybe (Result PrismicError (Response MyDocument))
    , prismic : Prismic.Types.Cache MyDocument
    , page : Page
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
    = NoOp
    | SetResponse ( Response MyDocument, Prismic.Types.Cache MyDocument )
    | SetError PrismicError
    | NavigateTo Page
    | ArticleMsg Article.Msg
    | BlogMsg Blog.Msg


type MyDocument
    = Default DefaultDocType
    | JobOfferDoc Documents.JobOffer
    | BlogPostDoc Documents.BlogPost
    | ArticleDoc Documents.Article
