module App.Documents.Types exposing (..)

import Prismic.Types exposing (PrismicError, Response, Api, StructuredText, Link, DefaultDocType, ImageField)


type alias Article =
    { content : StructuredText
    , image : ImageField
    , shortLede : StructuredText
    , title : StructuredText
    }


type alias BlogPost =
    { id : String
    , slugs : List String
    , body : StructuredText
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


type alias Product =
    { id : String
    , slugs : List String
    , allergens : Maybe String
    , color : String
    , description : StructuredText
    , flavour : Maybe (List String)
    , gallery : List ImageField
    , image : ImageField
    , name : StructuredText
    , price : Float
    , related : List Link
    , shortLede : StructuredText
    , testimonialAuthor : Maybe StructuredText
    , testimonialQuote : Maybe StructuredText
    , tags : List String
    }
