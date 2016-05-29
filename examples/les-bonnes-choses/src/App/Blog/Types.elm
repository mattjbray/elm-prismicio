module App.Blog.Types exposing (..)

import App.Blog.Index.Types as Index
import App.Blog.Post.Types as Post
import App.Documents.Types as Documents
import Prismic.Types exposing (PrismicError, Response, Api, StructuredText, Link, DefaultDocType, ImageField)

type Page
  = IndexP
  | PostP String

type Content
  = NoContent
  | IndexC Index.Model
  | PostC Post.Model

type Msg
  = NavigateTo Page
  | IndexMsg Index.Msg
  | PostMsg Post.Msg


type alias Model =
    { page : Page
    , content : Content
    }
