module App.Types exposing (..)

import Prismic.Types exposing (PrismicError, Response, Api, StructuredText, Link, DefaultDocType)


type alias Model =
    { response : Maybe (Result PrismicError (Response MyDocument))
    , api : Maybe (Result PrismicError Api)
    , selectedForm : String
    }


type Msg
    = NoOp
    | SetApi Api
    | SetResponse (Response MyDocument)
    | SetError PrismicError
    | SetSelectedForm String


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
