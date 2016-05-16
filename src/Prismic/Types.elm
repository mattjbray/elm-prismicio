module Prismic.Types exposing (..)

import Dict exposing (Dict)
import Http

type alias Cache docType = Cache' (Maybe Api) docType

type alias CacheWithApi docType = Cache' Api docType

type alias Cache' api docType =
    { api : api
    , url : Url
    , nextRequestId : Int
    , requests : Dict Int Query
    , responses : Dict Int (Response docType)
    }


-- API


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


type Ref
    = Ref String


type alias Form =
    { method : String
    , enctype : String
    , action : Url
    , fields : Dict String FormField
    , rel : Maybe String
    , name : Maybe String
    }


type alias FormField =
    { fieldType : FieldType
    , multiple : Bool
    , default : Maybe String
    }


type FieldType
    = String
    | Integer


type alias Experiments =
    -- TODO: Experiments are not Strings.  Fill out this type.
    { draft : List String
    , running : List String
    }



-- QUERY


type alias Query =
    { action : Url
    , ref : Ref
    , query : String
    }



-- ERRORS


type PrismicError
    = FormDoesNotExist String
    | RefDoesNotExist String
    | FetchApiError Http.Error
    | SubmitQueryError Http.Error



-- RESPONSE


type Url
    = Url String


type alias Response docType =
    { license : String
    , nextPage : Maybe Url
    , page : Int
    , prevPage : Maybe Url
    , results : List (SearchResult docType)
    , resultsPerPage : Int
    , resultsSize : Int
    , totalPages : Int
    , totalResultsSize : Int
    , version : String
    }


type alias DefaultDocType =
    Dict String (Dict String (List DocumentField))


type alias SearchResult docType =
    { data : docType
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
    | StructuredText StructuredText
    | Select String
    | Color String
    | Image ImageField
    | Number Float
    | Date String
    | Link Link


type alias StructuredText = List StructuredTextField


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
    | Hyperlink Link


type alias ImageField =
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


type EmbedProperties
  = EmbedVideo EmbedVideoProperties
  | EmbedRich EmbedRichProperties


type alias EmbedVideoProperties =
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
    , version : String
    , width : Int
    }


type alias EmbedRichProperties =
    { authorName : String
    , authorUrl : Url
    , cacheAge : String
    , embedUrl : Url
    , height : Maybe Int
    , html : String
    , providerName : String
    , providerUrl : Url
    , title : String
    , url : Url
    , version : String
    , width : Int
    }


type Link
    = DocumentLink LinkedDocument Bool
    | WebLink Url
