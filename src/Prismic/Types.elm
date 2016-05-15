module Prismic.Types exposing (..)

import Dict exposing (Dict)
import Http


type alias Api =
    { refs : List RefProperties
    , bookmarks : Dict String String
    , types : Dict String String
    , tags : List String
    , version : String
    , forms : Dict String Form
    , oauthInitiate : String
    , oauthToken : String
    , license : String
    , experiments : Experiments
    }


type alias RefProperties =
    { id : String
    , ref : Ref
    , label : String
    , isMasterRef : Bool
    }


type Ref = Ref String


type alias Form =
    { method : String
    , enctype : String
    , action : Url
    , fields : Dict String Field
    , rel : Maybe String
    , name : Maybe String
    }


type alias Field =
    { fieldType : FieldType
    , multiple : Bool
    , default : Maybe String
    }


type FieldType
    = String
    | Integer



-- TODO: Experiments are not Strings.  Fill out this type.


type alias Experiments =
    { draft : List String
    , running : List String
    }


type FetchFormError
    = FormDoesNotExist
    | RefDoesNotExist
    | HttpError Http.Error


type Url
    = Url String


type alias Response =
    { license : String
    , nextPage : Maybe Url
    , page : Int
    , prevPage : Maybe Url
    , results : List SearchResult
    , resultsPerPage : Int
    , resultsSize : Int
    , totalPages : Int
    , totalResultsSize : Int
    , version : String
    }


type alias SearchResult =
    { data : Dict String (Dict String (List DocumentField))
    , href : Url
    , id : String
    , linkedDocuments : List LinkedDocument
    , slugs : List String
    , tags : List String
    , resultType : String
    , uid : Maybe String
    }


type alias LinkedDocument =
    { id : String
    , slug : String
    , tags : List String
    , linkedDocumentType : String
    }


type DocumentField
    = Text String
    | StructuredText (List StructuredTextField)
    | Select String
    | Color String
    | Image ImageValue
    | Number Float
    | Date String
    | Link LinkField


type StructuredTextField
    = SSimple SimpleStructuredTextField
    | SImage ImageProperties
    | SEmbed EmbedProperties


type alias SimpleStructuredTextField =
    { fieldType : SimpleStructuredTextType
    , text : String
    , spans : List Span
    }


type SimpleStructuredTextType
    = Heading1
    | Heading2
    | Heading3
    | Paragraph
    | ListItem


type alias Span =
    { start : Int
    , end : Int
    , spanType : SpanType
    }


type SpanType
    = Em
    | Strong
    | Hyperlink LinkField


type alias ImageValue =
    { main : ImageProperties
    , views : Dict String ImageProperties
    }


type alias ImageProperties =
    { alt : Maybe String
    , copyright : Maybe String
    , url : Url
    , dimensions : ImageDimensions
    }


type alias ImageDimensions =
    { width : Int
    , height : Int
    }


type alias EmbedProperties =
    { authorName : String
    , authorUrl : Url
    , embedUrl : Url
    , height : Int
    , html : String
    , providerName : String
    , providerUrl : Url
    , thumbnailHeight : Int
    , thumbnailUrl : Url
    , thumbnailWidth : Int
    , title : String
    , embedType : String
    , version : String
    , width : Int
    }


type LinkField
    = DocumentLink LinkedDocument Bool
    | WebLink Url
