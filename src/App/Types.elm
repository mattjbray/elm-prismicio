module App.Types exposing (..)

import Prismic.Types exposing (PrismicError, Response, Api, StructuredText, Link, DefaultDocType)


type alias Model =
    { response : Maybe (Result PrismicError (Response MyDocument))
    , prismic : Prismic.Types.Cache MyDocument
    , selected : Selection
    }


type Selection
  = Form String
  | Bookmark String


type Msg
    = NoOp
    | SetResponse (Response MyDocument, Prismic.Types.Cache MyDocument)
    | SetError PrismicError
    | SetSelected Selection


type MyDocument
    = Default DefaultDocType
    | JobOfferDoc JobOffer
    | BlogPostDoc BlogPost


type alias BlogPost =
    { body : StructuredText
    , author : String
    , category : String
    , date : String
    , shortLede : StructuredText
    , relatedPosts : List Link
    , relatedProducts : List Link
    , allowComments : Bool
    }


type alias JobOffer =
    { name : StructuredText
    , contractType : Maybe String
    , service : Maybe String
    , jobDescription : StructuredText
    , profile : StructuredText
    , locations : List Link
    }
