module Prismic.Api
    exposing
        ( Api
        , Experiments
        , FieldType
        , Form
        , FormField
        , Ref(Ref)
        , RefProperties
        , Response
        , SearchResult
        , decodeApi
        , decodeResponse
        )

{-|


## Api

@docs Api, RefProperties, Ref, Form, FormField, FieldType, Experiments


## Response

@docs Response, SearchResult


## Internal

@docs decodeApi, decodeResponse

-}

import Dict exposing (Dict)
import Json.Decode as Json
import Json.Decode.Pipeline as Json exposing (custom, optional, required, requiredAt)
import Prismic.Document exposing (Document, DocumentReference, decodeDocumentJson, decodeDocumentReferenceJson)


-- Types: API


{-| The `Api` for your Prismic repository.

Your app can look things up in this if you need to (for example, to resolve
links using the bookmarks `Dict`).

-}
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


{-| Properties representing a Prismic ref.

Most of the time you will be working with the `master` ref, which is added to
all requests by default.

-}
type alias RefProperties =
    { id : String
    , ref : Ref
    , label : String
    , isMasterRef : Bool
    }


{-| A type to disambiguate `Ref`s from other `String`s.
-}
type Ref
    = Ref String


{-| Properties representing a Prismic form.

These are used to construct a default query.

-}
type alias Form =
    { method : String
    , enctype : String
    , action : String
    , fields : Dict String FormField
    , rel : Maybe String
    , name : Maybe String
    }


{-| A field in a Prismic form.

These are combined to construct query parameters for the eventual Http request.

-}
type alias FormField =
    { fieldType : FieldType
    , multiple : Bool
    , default : Maybe String
    }


{-| The type of values for a Prismic form field.
-}
type FieldType
    = String
    | Integer


{-| TODO: Experiments are not Strings. Fill out this type.
-}
type alias Experiments =
    { draft : List String
    , running : List String
    }



-- RESPONSE


{-| Represents a Prismic response.

This type is parameterized by `docType`, which is determined by the `Decoder`
you pass to `submit`.

-}
type alias Response docType =
    { license : String
    , nextPage : Maybe String
    , page : Int
    , prevPage : Maybe String
    , results : List (SearchResult docType)
    , resultsPerPage : Int
    , resultsSize : Int
    , totalPages : Int
    , totalResultsSize : Int
    , version : String
    }


{-| Represents a single document in a `Response`.

This type is parameterized by `docType`, which is determined by the `Decoder`
you pass to `submit`.

-}
type alias SearchResult docType =
    { data : docType
    , href : String
    , id : String
    , linkedDocuments : List DocumentReference
    , slugs : List String
    , tags : List String
    , resultType : String
    , uid : Maybe String
    }



-- DECODERS


decodeRef : Json.Decoder Ref
decodeRef =
    Json.map Ref Json.string


{-| Decode an `Api` from JSON.
-}
decodeApi : Json.Decoder Api
decodeApi =
    Json.decode Api
        |> required "refs" (Json.list decodeRefProperties)
        |> required "bookmarks" (Json.dict Json.string)
        |> required "types" (Json.dict Json.string)
        |> required "tags" (Json.list Json.string)
        |> required "version" Json.string
        |> required "forms" (Json.dict decodeForm)
        |> required "oauth_initiate" Json.string
        |> required "oauth_token" Json.string
        |> required "license" Json.string
        |> required "experiments" decodeExperiments


decodeRefProperties : Json.Decoder RefProperties
decodeRefProperties =
    Json.decode RefProperties
        |> required "id" Json.string
        |> required "ref" decodeRef
        |> required "label" Json.string
        |> optional "isMasterRef" Json.bool False


decodeForm : Json.Decoder Form
decodeForm =
    Json.decode Form
        |> required "method" Json.string
        |> required "enctype" Json.string
        |> required "action" Json.string
        |> required "fields" (Json.dict decodeFormField)
        |> optional "rel" (Json.maybe Json.string) Nothing
        |> optional "name" (Json.maybe Json.string) Nothing


decodeFormField : Json.Decoder FormField
decodeFormField =
    Json.decode FormField
        |> required "type" decodeFieldType
        |> required "multiple" Json.bool
        |> optional "default" (Json.maybe Json.string) Nothing


decodeFieldType : Json.Decoder FieldType
decodeFieldType =
    let
        decodeOnType str =
            case str of
                "String" ->
                    Json.succeed String

                "Integer" ->
                    Json.succeed Integer

                _ ->
                    Json.fail ("Unknown field type: " ++ str)
    in
    Json.string |> Json.andThen decodeOnType


decodeExperiments : Json.Decoder Experiments
decodeExperiments =
    Json.decode Experiments
        |> required "draft" (Json.list Json.string)
        |> required "running" (Json.list Json.string)


{-| Decode a `Response` from JSON.
-}
decodeResponse : Json.Decoder (Response Document)
decodeResponse =
    Json.decode Response
        |> required "license" Json.string
        |> required "next_page" (Json.nullable Json.string)
        |> required "page" Json.int
        |> required "prev_page" (Json.nullable Json.string)
        |> required "results" (Json.list decodeSearchResult)
        |> required "results_per_page" Json.int
        |> required "results_size" Json.int
        |> required "total_pages" Json.int
        |> required "total_results_size" Json.int
        |> required "version" Json.string


decodeSearchResult : Json.Decoder (SearchResult Document)
decodeSearchResult =
    Json.decode SearchResult
        |> custom decodeDocumentJson
        |> required "href" Json.string
        |> required "id" Json.string
        |> required "linked_documents" (Json.list decodeDocumentReferenceJson)
        |> required "slugs" (Json.list Json.string)
        |> required "tags" (Json.list Json.string)
        |> required "type" Json.string
        |> required "uid" (Json.nullable Json.string)
