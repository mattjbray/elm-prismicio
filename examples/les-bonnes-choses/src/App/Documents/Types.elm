module App.Documents.Types exposing (..)

import Prismic as P


type alias Article =
    { id : String
    , content : P.StructuredText
    , image : P.ImageViews
    , shortLede : P.StructuredText
    , title : P.StructuredText
    }


type alias BlogPost =
    { id : String
    , slugs : List String
    , body : P.StructuredText
    , author : String
    , category : String
    , date : String
    , shortLede : P.StructuredText
    , relatedPosts : List P.Link
    , relatedProducts : List P.Link
    , allowComments : Bool
    }


type alias JobOffer =
    { id : String
    , slugs : List String
    , tags : List String
    , name : P.StructuredText
    , contractType : Maybe String
    , service : Maybe String
    , jobDescription : P.StructuredText
    , profile : P.StructuredText
    , locations : List P.Link
    }


type Category
    = Macaron
    | Cupcake
    | Pie


type alias Product =
    { id : String
    , slugs : List String
    , allergens : Maybe String
    , color : String
    , description : P.StructuredText
    , flavours : List String
    , gallery : List P.ImageViews
    , image : P.ImageViews
    , name : P.StructuredText
    , price : Float
    , related : List P.Link
    , shortLede : P.StructuredText
    , testimonialAuthor : Maybe P.StructuredText
    , testimonialQuote : Maybe P.StructuredText
    , tags : List String
    , categories : List Category
    }


type alias Selection =
    { id : String
    , slugs : List String
    , tags : List String
    , name : P.StructuredText
    , catcherImage : P.ImageViews
    , description : P.StructuredText
    , image : P.ImageViews
    , price : Float
    , products : List P.Link
    , shortLede : P.StructuredText
    }


type alias Store =
    { id : String
    , slugs : List String
    , tags : List String
    , address : String
    , city : String
    , zipcode : String
    , country : String
    , description : P.StructuredText
    , name : P.StructuredText
    , image : P.ImageViews
    , monday : List String
    , tuesday : List String
    , wednesday : List String
    , thursday : List String
    , friday : List String
    , saturday : List String
    , sunday : List String
    }
