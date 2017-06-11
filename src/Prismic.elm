module Prismic
    exposing
        ( Api
        , Block
        , Decoder
        , Document
        , DocumentReference
        , Embed(..)
        , EmbedRich
        , EmbedVideo
        , Experiments
        , FieldType
        , Form
        , FormField
        , ImageDimensions
        , ImageView
        , ImageViews
        , Link(DocumentLink, WebLink)
        , Model
        , ModelWithApi
        , Model_
        , Predicate
        , PrismicError(..)
        , Ref(Ref)
        , RefProperties
        , Request
        , Response
        , Span
        , SpanElement(..)
        , StructuredText
        , StructuredTextBlock(..)
        , Url(Url)
        , any
        , api
        , at
        , atL
        , bookmark
        , collectResponses
        , decode
        , defaultLinkResolver
        , field
        , form
        , fulltext
        , getFirstImage
        , getFirstParagraph
        , getText
        , getTexts
        , getTitle
        , group
        , image
        , init
        , map
        , none
        , query
        , ref
        , slice
        , sliceZone
        , structuredText
        , structuredTextAsHtml
        , submit
        , text
        )

{-| An Elm SDK for [Prismic.io](https://prismic.io).


# Initialisation

@docs init


# Making a request

@docs api, form, bookmark, submit, collectResponses


# Customising the request

@docs ref, query, none


# Predicates

@docs at, atL, any, fulltext


# Types


## Models

@docs Url, Model, ModelWithApi, Model_


## Errors

@docs PrismicError


## Api

@docs Api, RefProperties, Ref, Form, FormField, FieldType, Experiments


## Requests

@docs Predicate, Request


## Response

@docs Response


## Documents

@docs Document, Decoder, decode, map, field, text, structuredText, image, sliceZone, slice, group


### Field types

You can create your own Elm types to represent your documents using the
following components.


#### Structured Text

@docs StructuredText, StructuredTextBlock, Block, Span, SpanElement


#### Image

@docs ImageViews, ImageView, ImageDimensions


#### Embed

@docs Embed, EmbedRich, EmbedVideo


#### Link

@docs Link, DocumentReference


## Viewing documents

@docs structuredTextAsHtml
@docs defaultLinkResolver


### `StructuredText` helpers

@docs getTitle, getFirstImage, getFirstParagraph, getText, getTexts

-}

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (href, property, src)
import Http
import Json.Decode as Json
import Json.Decode.Pipeline as JDP exposing (custom, optional, requiredAt)
import Json.Encode
import Result.Extra as Result
import String
import Task exposing (Task)
import Task.Extra as Task


-- Types: Models


{-| Disambiguate `Url`s from `String`s
-}
type Url
    = Url String


{-| This is the main user-facing type for elm-prismicio's internal state.

The `Api` is represented as `Maybe Api`, because we may not have fetched it yet.

-}
type alias Model =
    Model_ (Maybe Api)


{-| This variation of the Model type is returned by `api`, when we know we have successfully retreived the `Api`.

It is used internally by elm-prismicio.

-}
type alias ModelWithApi =
    Model_ Api


{-| The generic `Model'` type, where the `Api` is represented by a type parameter.

You will be using the specialised `Model` type in user code.

-}
type alias Model_ api =
    { api : api
    , url : Url
    , nextRequestId : Int
    , cache : Dict String (Response Document)
    }



-- Types: Errors


{-| The possible errors elm-prismicio raises.
-}
type PrismicError
    = FormDoesNotExist String
    | RefDoesNotExist String
    | BookmarkDoesNotExist String
    | FetchApiError Http.Error
    | SubmitRequestError Http.Error
    | DecodeDocumentError String



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
    , action : Url
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



-- REQUEST


{-| The type representing Prismic query predicates.
-}
type Predicate
    = At String String
    | AtL String (List String)
    | Any String (List String)
    | FullText String String


{-| Represents a Prismic request.
-}
type alias Request =
    { action : Url
    , ref : Ref
    , q : String
    }



-- RESPONSE


{-| Represents a Prismic response.

This type is parameterized by `docType`, which is determined by the `Decoder`
you pass to `submit`.

-}
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


{-| Represents a single document in a `Response`.

This type is parameterized by `docType`, which is determined by the `Json.Decoder`
you pass to `submit`.

-}
type alias SearchResult docType =
    { data : docType
    , href : Url
    , id : String
    , linkedDocuments : List DocumentReference
    , slugs : List String
    , tags : List String
    , resultType : String
    , uid : Maybe String
    }


{-| Holds the Prismic document.

You will decode this into your own document type by passing a `Decoder MyDoc` to
`submit`.

-}
type Document
    = Document (Dict String DocumentField)


{-| A field in the `Document`.
-}
type DocumentField
    = Text String
    | StructuredText StructuredText
    | Select String
    | Color String
    | Image ImageViews
    | Number Float
    | Date String
    | Link Link
    | SliceZone SliceZone
    | Groups (List Group)



--  DOCUMENT DECODERS


{-| A value that knows how to decode Documents.

Construct a `Decoder` to pass to `submit`.

-}
type Decoder a
    = Decoder (Document -> Result String a)


type FieldDecoder a
    = FieldDecoder (DocumentField -> Result String a)


{-| Begin a decoding pipeline.

    type alias MyDoc =
        { title : StructuredText }

    myDocDecoder : Decoder MyDoc
    myDocDecoder =
        decode MyDoc
            |> field "title" structuredText

-}
decode : a -> Decoder a
decode doc =
    Decoder (\_ -> Ok doc)


decodeValue : Decoder a -> Document -> Result String a
decodeValue (Decoder decoder) doc =
    decoder doc


{-| Transform a decoder.
-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decoder) =
    Decoder
        (\doc ->
            decoder doc |> Result.map f
        )


apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply (Decoder f) (Decoder a) =
    Decoder
        (\doc ->
            case ( f doc, a doc ) of
                ( Ok g, Ok x ) ->
                    Ok (g x)

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err
        )


{-| Decode a field.
-}
field : String -> FieldDecoder a -> Decoder (a -> b) -> Decoder b
field key valDecoder decoder =
    apply decoder (fieldKey key valDecoder)


fieldKey : String -> FieldDecoder a -> Decoder a
fieldKey key (FieldDecoder fieldDecoder) =
    Decoder
        (\(Document doc) ->
            case Dict.get key doc of
                Just field ->
                    fieldDecoder field
                        |> Result.mapError (\msg -> "While decoding field '" ++ key ++ "': " ++ msg)

                Nothing ->
                    Err ("No field at " ++ key)
        )


{-| Decode a Text field.
-}
text : FieldDecoder String
text =
    FieldDecoder
        (\field ->
            case field of
                Text text ->
                    Ok text

                _ ->
                    Err ("Expected a Text field, but got '" ++ toString field ++ "'.")
        )


{-| Decode a StructuredText field.
-}
structuredText : FieldDecoder StructuredText
structuredText =
    FieldDecoder
        (\field ->
            case field of
                StructuredText x ->
                    Ok x

                _ ->
                    Err ("Expected a StructuredText field, but got '" ++ toString field ++ "'.")
        )


{-| Decode an Image field.
-}
image : FieldDecoder ImageViews
image =
    FieldDecoder
        (\field ->
            case field of
                Image x ->
                    Ok x

                _ ->
                    Err ("Expected an Image field, but got '" ++ toString field ++ "'.")
        )


type SliceDecoder a
    = SliceDecoder (Slice -> Result String a)


oneOf : List (SliceDecoder a) -> Slice -> Result String a
oneOf sliceDecoders slice =
    let
        go decoders errors =
            case decoders of
                [] ->
                    Err
                        ("No slices matched: \n* "
                            ++ String.join "\n* " errors
                        )

                (SliceDecoder decoder) :: rest ->
                    case decoder slice of
                        Ok x ->
                            Ok x

                        Err err ->
                            go rest (err :: errors)
    in
    go sliceDecoders []


{-| Decode a Slice field.
-}
slice : String -> (a -> b) -> FieldDecoder a -> SliceDecoder b
slice sliceType tagger (FieldDecoder fieldDecoder) =
    SliceDecoder
        (\slice ->
            if sliceType == slice.sliceType then
                fieldDecoder slice.sliceField
                    |> Result.map tagger
                    |> Result.mapError
                        (\msg -> "While decoding slice with type '" ++ slice.sliceType ++ "': " ++ msg)
            else
                Err ("Expected slice with type '" ++ sliceType ++ "' but got '" ++ slice.sliceType ++ "'.")
        )


{-| Decode a SliceZone.

Pass this function a list of possible elements that can appear in the Slice.

    type alias MyDoc =
        { section : Section }

    type Section
        = MyContent StructuredText
        | MyImage ImageViews

    myDocDecoder : Decode MyDoc
    myDocDecoder =
        decode MyDoc
            |> field "section"
                (sliceZone
                    [ slice "theContent" MyContent structuredText
                    , slice "theImage" MyImage image
                    ]
                )

-}
sliceZone : List (SliceDecoder a) -> FieldDecoder (List a)
sliceZone decoders =
    FieldDecoder
        (\field ->
            case field of
                SliceZone slices ->
                    slices
                        |> List.map (oneOf decoders)
                        |> Result.collect

                _ ->
                    Err "Expected a SliceZone field."
        )


{-| Decode a group.

Groups are essentially Documents, so you pass `group` a Document `Decoder`.
 -}
group : Decoder a -> FieldDecoder (List a)
group decoder =
    FieldDecoder
        (\field ->
            case field of
                Groups groups ->
                    groups
                        |> List.map Document
                        |> List.map (decodeValue decoder)
                        |> Result.collect

                _ ->
                    Err ("Expected a Group field, but got '" ++ toString field ++ "'.")
        )


{-| `StructuredText` is a list of `StructuredTextBlock`s.
-}
type alias StructuredText =
    List StructuredTextBlock


{-| An element of `StructuredText`.
-}
type StructuredTextBlock
    = Heading1 Block
    | Heading2 Block
    | Heading3 Block
    | Paragraph Block
    | ListItem Block
    | SImage ImageView
    | SEmbed Embed


{-| Contents of `StructuredText` blocks, such as headings and paragraphs.
-}
type alias Block =
    { text : String
    , spans : List Span
    }


{-| `Span`s are nested within `StructuredText` blocks.
-}
type alias Span =
    { start : Int
    , end : Int
    , spanElement : SpanElement
    }


{-| Types of spans.
-}
type SpanElement
    = Em
    | Strong
    | Hyperlink Link


{-| A collection of image views.
-}
type alias ImageViews =
    { main : ImageView
    , views : Dict String ImageView
    }


{-| Properties for a single image view.
-}
type alias ImageView =
    { alt : Maybe String
    , copyright : Maybe String
    , url : Url
    , dimensions : ImageDimensions
    }


{-| Dimensions of an image view.
-}
type alias ImageDimensions =
    { width : Int
    , height : Int
    }


{-| Embed elements.
-}
type Embed
    = EVideo EmbedVideo
    | ERich EmbedRich


{-| Video embed elements.
-}
type alias EmbedVideo =
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


{-| Rich embed elements.
-}
type alias EmbedRich =
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


{-| Links to other documents or to the web.
-}
type Link
    = DocumentLink DocumentReference Bool
    | WebLink Url


{-| A referenced to a Prismic document.
-}
type alias DocumentReference =
    { id : String
    , slug : String
    , tags : List String
    , linkedDocumentType : String
    }


{-| Contains zero or more `Slice`s.
-}
type alias SliceZone =
    List Slice


{-| A Slice
-}
type alias Slice =
    { sliceLabel : Maybe String
    , sliceType : String
    , sliceField :
        DocumentField

    -- TODO: SliceField to exclude nested Slices?
    }


type alias Group =
    Dict String DocumentField



-- FUNCTIONS


{-| Initialise the Prismic model with the URL for your Prismic repository. Save
this in your application's Model somewhere.

    type alias Model =
        { prismic : Prismic.Model }

    init =
        { prismic =
            Prismic.init (Url "https://lesbonneschoses.prismic.io/api")
        }

-}
init : Url -> Model
init url =
    { api = Nothing
    , url = url
    , nextRequestId = 0
    , cache = Dict.empty
    }


{-| Go and fetch the Prismic API, if it has not already been fetched. You must
start every Prismic request with this function.
-}
api : Model -> Task PrismicError ModelWithApi
api cache =
    case cache.api of
        Just api ->
            Task.succeed { cache | api = api }

        Nothing ->
            let
                (Url url) =
                    cache.url
            in
            Task.map (\api -> { cache | api = api })
                (Task.mapError FetchApiError
                    (Http.get url decodeApi |> Http.toTask)
                )


{-| Choose a form on which to base the rest of the Prismic request.
-}
form :
    String
    -> Task PrismicError ModelWithApi
    -> Task PrismicError ( Request, ModelWithApi )
form formId apiTask =
    let
        addForm cache =
            let
                mForm =
                    Dict.get formId cache.api.forms

                defaultRefId =
                    "master"

                mRef =
                    getRefById defaultRefId cache.api
            in
            case ( mForm, mRef ) of
                ( Nothing, _ ) ->
                    Task.fail (FormDoesNotExist formId)

                ( _, Nothing ) ->
                    Task.fail (RefDoesNotExist defaultRefId)

                ( Just form, Just masterRef ) ->
                    let
                        q =
                            Maybe.withDefault ""
                                (Dict.get "q" form.fields
                                    |> Maybe.andThen .default
                                )
                    in
                    Task.succeed
                        ( { action = form.action
                          , ref = masterRef.ref
                          , q = q
                          }
                        , cache
                        )
    in
    apiTask |> Task.andThen addForm


{-| Convenience function for fetching a bookmarked document.
-}
bookmark :
    String
    -> Task PrismicError ModelWithApi
    -> Task PrismicError ( Request, ModelWithApi )
bookmark bookmarkId cacheTask =
    cacheTask
        |> Task.andThen
            (\cacheWithApi ->
                let
                    mDocId =
                        Dict.get bookmarkId cacheWithApi.api.bookmarks
                in
                case mDocId of
                    Nothing ->
                        Task.fail (BookmarkDoesNotExist bookmarkId)

                    Just docId ->
                        Task.succeed cacheWithApi
                            |> form "everything"
                            |> query [ at "document.id" docId ]
            )


{-| Override a Form's default ref
-}
ref :
    String
    -> Task PrismicError ( Request, ModelWithApi )
    -> Task PrismicError ( Request, ModelWithApi )
ref refId requestTask =
    let
        addRef ( request, cache ) =
            case getRefById refId cache.api of
                Nothing ->
                    Task.fail (RefDoesNotExist refId)

                Just r ->
                    Task.succeed
                        ( { request | ref = r.ref }
                        , cache
                        )
    in
    requestTask |> Task.andThen addRef


{-| Override a Form's default query.

See the section on `Predicate`s below for how to construct a `Predicate`.

-}
query :
    List Predicate
    -> Task PrismicError ( Request, ModelWithApi )
    -> Task PrismicError ( Request, ModelWithApi )
query predicates requestTask =
    let
        addQuery ( request, cache ) =
            Task.succeed
                ( { request | q = predicatesToStr predicates }
                , cache
                )
    in
    requestTask |> Task.andThen addQuery


{-| Pass the request through unmodified.

Useful for conditionally adding a query.

-}
none :
    Task PrismicError ( Request, Model_ api )
    -> Task PrismicError ( Request, Model_ api )
none =
    Task.map identity


{-| Submit the request.

Pass this function a `Decoder` to decode each document in the response into your
own Elm document type.

-}
submit :
    Decoder docType
    -> Task PrismicError ( Request, ModelWithApi )
    -> Task PrismicError ( Response docType, Model )
submit decodeDocType requestTask =
    let
        doSubmit ( request, cache ) =
            let
                (Url url) =
                    requestToUrl request

                cacheWithApi =
                    { cache | api = Just cache.api }

                decodeResponseToUserDocType response =
                    response.results
                        |> List.map
                            (\result ->
                                decodeValue decodeDocType result.data
                                    |> Result.map (\doc -> { result | data = doc })
                            )
                        |> Result.collect
                        |> Result.map (\docs -> { response | results = docs })
                        |> Task.fromResult
                        |> Task.mapError (\msg -> DecodeDocumentError msg)
            in
            case getFromCache request cache of
                Just response ->
                    decodeResponseToUserDocType response
                        |> Task.map (\response -> ( response, cacheWithApi ))

                Nothing ->
                    Http.get url decodeResponse
                        |> Http.toTask
                        |> Task.mapError SubmitRequestError
                        |> Task.andThen
                            (\origResponse ->
                                decodeResponseToUserDocType origResponse
                                    |> Task.map
                                        (\response ->
                                            ( response
                                            , setInCache request origResponse cacheWithApi
                                            )
                                        )
                            )
    in
    requestTask |> Task.andThen doSubmit


{-| The `submit` `Task` returns an updated Prismic `Model` with the request and
response cached.

In your app's `update` function, you should merge this with the existing cache
using `collectResponses`.

    update msg model =
        case msg of
            MyPrismicMsg (Ok ( response, prismic )) ->
                { model
                    | prismic =
                        collectResponses model.prismic prismic
                }

-}
collectResponses : Model -> Model -> Model
collectResponses model1 model2 =
    { model2
        | cache = Dict.union model2.cache model1.cache
    }



-- Predicates


{-| Match documents having `value` at `fragment`.
-}
at : String -> String -> Predicate
at fragment value =
    At fragment value


{-| Match documents having a list of `values` at `fragment`.
-}
atL : String -> List String -> Predicate
atL fragment values =
    AtL fragment values


{-| Match documents having any of `values` at `fragment`.
-}
any : String -> List String -> Predicate
any fragment values =
    Any fragment values


{-| Match documents with a full text search at `fragment`.
-}
fulltext : String -> String -> Predicate
fulltext fragment value =
    FullText fragment value



-- DECODER HELPERS


maybeWithDefault : a -> Json.Decoder a -> Json.Decoder a
maybeWithDefault default decoder =
    Json.maybe decoder |> Json.andThen (Json.succeed << Maybe.withDefault default)


decodeRef : Json.Decoder Ref
decodeRef =
    Json.map Ref Json.string


decodeUrl : Json.Decoder Url
decodeUrl =
    Json.map Url Json.string



-- DECODERS


decodeApi : Json.Decoder Api
decodeApi =
    JDP.decode Api
        |> JDP.required "refs" (Json.list decodeRefProperties)
        |> JDP.required "bookmarks" (Json.dict Json.string)
        |> JDP.required "types" (Json.dict Json.string)
        |> JDP.required "tags" (Json.list Json.string)
        |> JDP.required "version" Json.string
        |> JDP.required "forms" (Json.dict decodeForm)
        |> JDP.required "oauth_initiate" Json.string
        |> JDP.required "oauth_token" Json.string
        |> JDP.required "license" Json.string
        |> JDP.required "experiments" decodeExperiments


decodeRefProperties : Json.Decoder RefProperties
decodeRefProperties =
    JDP.decode RefProperties
        |> JDP.required "id" Json.string
        |> JDP.required "ref" decodeRef
        |> JDP.required "label" Json.string
        |> optional "isMasterRef" Json.bool False


decodeForm : Json.Decoder Form
decodeForm =
    JDP.decode Form
        |> JDP.required "method" Json.string
        |> JDP.required "enctype" Json.string
        |> JDP.required "action" decodeUrl
        |> JDP.required "fields" (Json.dict decodeFormField)
        |> optional "rel" (Json.maybe Json.string) Nothing
        |> optional "name" (Json.maybe Json.string) Nothing


decodeFormField : Json.Decoder FormField
decodeFormField =
    JDP.decode FormField
        |> JDP.required "type" decodeFieldType
        |> JDP.required "multiple" Json.bool
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
    JDP.decode Experiments
        |> JDP.required "draft" (Json.list Json.string)
        |> JDP.required "running" (Json.list Json.string)


decodeResponse : Json.Decoder (Response Document)
decodeResponse =
    JDP.decode Response
        |> JDP.required "license" Json.string
        |> JDP.required "next_page" (Json.nullable decodeUrl)
        |> JDP.required "page" Json.int
        |> JDP.required "prev_page" (Json.nullable decodeUrl)
        |> JDP.required "results" (Json.list decodeSearchResult)
        |> JDP.required "results_per_page" Json.int
        |> JDP.required "results_size" Json.int
        |> JDP.required "total_pages" Json.int
        |> JDP.required "total_results_size" Json.int
        |> JDP.required "version" Json.string


{-| Decode a result to a `Document`.
-}
decodeDocument : Json.Decoder Document
decodeDocument =
    Json.field "type" Json.string
        |> Json.andThen
            (\docType ->
                Json.at [ "data", docType ] (Json.dict decodeDocumentField)
            )
        |> Json.map Document


decodeSearchResult : Json.Decoder (SearchResult Document)
decodeSearchResult =
    JDP.decode SearchResult
        |> custom decodeDocument
        |> JDP.required "href" decodeUrl
        |> JDP.required "id" Json.string
        |> JDP.required "linked_documents" (Json.list decodeDocumentReference)
        |> JDP.required "slugs" (Json.list Json.string)
        |> JDP.required "tags" (Json.list Json.string)
        |> JDP.required "type" Json.string
        |> JDP.required "uid" (Json.nullable Json.string)


decodeDocumentReference : Json.Decoder DocumentReference
decodeDocumentReference =
    JDP.decode DocumentReference
        |> JDP.required "id" Json.string
        |> JDP.required "slug" Json.string
        |> JDP.required "tags" (Json.list Json.string)
        |> JDP.required "type" Json.string


decodeDocumentField : Json.Decoder DocumentField
decodeDocumentField =
    let
        decodeOnType typeStr =
            case typeStr of
                "Text" ->
                    Json.map Text (Json.field "value" Json.string)

                "Select" ->
                    Json.map Select (Json.field "value" Json.string)

                "Color" ->
                    Json.map Color (Json.field "value" Json.string)

                "Number" ->
                    Json.map Number (Json.field "value" Json.float)

                "Date" ->
                    Json.map Date (Json.field "value" Json.string)

                "Image" ->
                    Json.map Image (Json.field "value" decodeImageViews)

                "StructuredText" ->
                    Json.map StructuredText (Json.field "value" decodeStructuredText)

                "Link.document" ->
                    Json.map Link decodeLink

                "Link.web" ->
                    Json.map Link decodeLink

                "SliceZone" ->
                    Json.map SliceZone (Json.field "value" decodeSliceZone)

                "Group" ->
                    Json.map Groups (Json.field "value" (Json.list (Json.dict (Json.lazy (\_ -> decodeDocumentField)))))

                _ ->
                    Json.fail ("Unknown document field type: " ++ typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


{-| Decode some `StructuredText`.
-}
decodeStructuredText : Json.Decoder StructuredText
decodeStructuredText =
    Json.list decodeStructuredTextBlock


{-| Decode an `ImageField`.
-}
decodeImageViews : Json.Decoder ImageViews
decodeImageViews =
    JDP.decode ImageViews
        |> JDP.required "main" decodeImageView
        |> JDP.required "views" (Json.dict decodeImageView)


decodeImageView : Json.Decoder ImageView
decodeImageView =
    JDP.decode ImageView
        |> JDP.required "alt" (Json.nullable Json.string)
        |> JDP.required "copyright" (Json.nullable Json.string)
        |> JDP.required "url" decodeUrl
        |> JDP.required "dimensions" decodeImageDimensions


decodeImageDimensions : Json.Decoder ImageDimensions
decodeImageDimensions =
    JDP.decode ImageDimensions
        |> JDP.required "width" Json.int
        |> JDP.required "height" Json.int


decodeStructuredTextBlock : Json.Decoder StructuredTextBlock
decodeStructuredTextBlock =
    let
        decodeOnType typeStr =
            case typeStr of
                "heading1" ->
                    Json.map Heading1 decodeBlock

                "heading2" ->
                    Json.map Heading2 decodeBlock

                "heading3" ->
                    Json.map Heading3 decodeBlock

                "paragraph" ->
                    Json.map Paragraph decodeBlock

                "list-item" ->
                    Json.map ListItem decodeBlock

                "image" ->
                    Json.map SImage decodeImageView

                "embed" ->
                    Json.map SEmbed (Json.field "oembed" decodeEmbed)

                _ ->
                    Json.fail ("Unknown structured field type: " ++ toString typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


decodeBlock : Json.Decoder Block
decodeBlock =
    JDP.decode Block
        |> JDP.required "text" Json.string
        |> JDP.required "spans" (Json.list decodeSpan)


decodeSpan : Json.Decoder Span
decodeSpan =
    JDP.decode Span
        |> JDP.required "start" Json.int
        |> JDP.required "end" Json.int
        |> custom decodeSpanType


decodeSpanType : Json.Decoder SpanElement
decodeSpanType =
    let
        decodeOnType typeStr =
            case typeStr of
                "em" ->
                    Json.succeed Em

                "strong" ->
                    Json.succeed Strong

                "hyperlink" ->
                    Json.map Hyperlink (Json.field "data" decodeLink)

                _ ->
                    Json.fail ("Unknown span type: " ++ typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


{-| Decode an `Embed` field.
-}
decodeEmbed : Json.Decoder Embed
decodeEmbed =
    let
        decodeOnType typeStr =
            case typeStr of
                "video" ->
                    Json.map EVideo decodeEmbedVideo

                "rich" ->
                    Json.map ERich decodeEmbedRich

                _ ->
                    Json.fail ("Unknown embed type: " ++ typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


decodeEmbedVideo : Json.Decoder EmbedVideo
decodeEmbedVideo =
    JDP.decode EmbedVideo
        |> JDP.required "author_name" Json.string
        |> JDP.required "author_url" decodeUrl
        |> JDP.required "embed_url" decodeUrl
        |> JDP.required "height" Json.int
        |> JDP.required "html" Json.string
        |> JDP.required "provider_name" Json.string
        |> JDP.required "provider_url" decodeUrl
        |> JDP.required "thumbnail_height" Json.int
        |> JDP.required "thumbnail_url" decodeUrl
        |> JDP.required "thumbnail_width" Json.int
        |> JDP.required "title" Json.string
        |> JDP.required "version" Json.string
        |> JDP.required "width" Json.int


decodeEmbedRich : Json.Decoder EmbedRich
decodeEmbedRich =
    JDP.decode EmbedRich
        |> JDP.required "author_name" Json.string
        |> JDP.required "author_url" decodeUrl
        |> JDP.required "cache_age" Json.string
        |> JDP.required "embed_url" decodeUrl
        |> JDP.required "height" (Json.maybe Json.int)
        |> JDP.required "html" Json.string
        |> JDP.required "provider_name" Json.string
        |> JDP.required "provider_url" decodeUrl
        |> JDP.required "title" Json.string
        |> JDP.required "url" decodeUrl
        |> JDP.required "version" Json.string
        |> JDP.required "width" Json.int


{-| Decode a `Link`.
-}
decodeLink : Json.Decoder Link
decodeLink =
    let
        decodeOnType typeStr =
            case typeStr of
                "Link.document" ->
                    JDP.decode DocumentLink
                        |> requiredAt [ "value", "document" ] decodeDocumentReference
                        |> requiredAt [ "value", "isBroken" ] Json.bool

                "Link.web" ->
                    JDP.decode WebLink
                        |> requiredAt [ "value", "url" ] decodeUrl

                _ ->
                    Json.fail ("Unknown link type: " ++ typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


decodeSliceZone : Json.Decoder SliceZone
decodeSliceZone =
    Json.list (Json.lazy (\_ -> decodeSlice))


decodeSlice : Json.Decoder Slice
decodeSlice =
    JDP.decode Slice
        |> optional "slice_label" (Json.maybe Json.string) Nothing
        |> JDP.required "slice_type" Json.string
        |> JDP.required "value" decodeDocumentField



-- Html


asHtmlWithDefault :
    (DocumentReference -> Url)
    -> Html msg
    -> String
    -> String
    -> Dict String (Dict String (List DocumentField))
    -> Html msg
asHtmlWithDefault linkResolver default documentType fieldName data =
    Maybe.withDefault default
        (Dict.get documentType data
            |> Maybe.andThen (Dict.get fieldName)
            |> Maybe.andThen
                (\docs ->
                    Just
                        (case docs of
                            [ doc ] ->
                                fieldAsHtml linkResolver doc

                            _ ->
                                div [] (List.map (fieldAsHtml linkResolver) docs)
                        )
                )
        )


fieldAsHtml : (DocumentReference -> Url) -> DocumentField -> Html msg
fieldAsHtml linkResolver field =
    case field of
        Text t ->
            span [] [ Html.text t ]

        Date t ->
            span [] [ Html.text t ]

        Number n ->
            span [] [ Html.text (toString n) ]

        Select t ->
            span [] [ Html.text t ]

        Color t ->
            span [] [ Html.text ("<Color> " ++ t) ]

        Link l ->
            linkAsHtml linkResolver l

        Image i ->
            imageAsHtml i.main

        StructuredText fields ->
            div [] (structuredTextAsHtml linkResolver fields)

        SliceZone slices ->
            div [] (List.map (sliceAsHtml linkResolver) slices)

        Groups groups ->
            div []
                (groups
                    |> List.map
                        (\group ->
                            div [] (Dict.values group |> List.map (fieldAsHtml linkResolver))
                        )
                )


{-| Render some `StructuredText` as HTML.

You must supply a `linkResolver` to resolve any links in the `StructuredText`.
If you don't care about this, you can use the `defaultLinkResolver`.

-}
structuredTextAsHtml : (DocumentReference -> Url) -> StructuredText -> List (Html msg)
structuredTextAsHtml linkResolver =
    List.map (structuredTextBlockAsHtml linkResolver)


structuredTextBlockAsHtml : (DocumentReference -> Url) -> StructuredTextBlock -> Html msg
structuredTextBlockAsHtml linkResolver field =
    case field of
        SImage image ->
            imageAsHtml image

        SEmbed embed ->
            embedAsHtml embed

        Heading1 block ->
            blockAsHtml h1 linkResolver block

        Heading2 block ->
            blockAsHtml h2 linkResolver block

        Heading3 block ->
            blockAsHtml h3 linkResolver block

        Paragraph block ->
            blockAsHtml p linkResolver block

        ListItem block ->
            blockAsHtml
                (\attrs childs ->
                    ul [] [ li attrs childs ]
                )
                linkResolver
                block


blockAsHtml :
    (List (Attribute msg)
     -> List (Html msg)
     -> Html msg
    )
    -> (DocumentReference -> Url)
    -> Block
    -> Html msg
blockAsHtml el linkResolver field =
    let
        spanEl span =
            case span.spanElement of
                Em ->
                    em []

                Strong ->
                    strong []

                Hyperlink link ->
                    linkAsHtmlWith linkResolver link

        foldFn span ( childs, index ) =
            let
                beginning =
                    String.slice index span.start field.text

                middle =
                    String.slice span.start span.end field.text
            in
            ( childs ++ [ Html.text beginning, spanEl span [ Html.text middle ] ]
            , span.end
            )
    in
    el []
        (field.spans
            |> List.sortBy .start
            |> List.foldl foldFn ( [], 0 )
            |> (\( childs, index ) -> childs ++ [ Html.text (String.dropLeft index field.text) ])
        )


imageAsHtml : ImageView -> Html msg
imageAsHtml image =
    let
        (Url urlStr) =
            image.url
    in
    img [ src urlStr ] []


embedAsHtml : Embed -> Html msg
embedAsHtml embed =
    case embed of
        EVideo props ->
            div [ property "innerHTML" (Json.Encode.string props.html) ] []

        ERich props ->
            div [ property "innerHTML" (Json.Encode.string props.html) ] []


linkAsHtml : (DocumentReference -> Url) -> Link -> Html msg
linkAsHtml linkResolver link =
    case link of
        DocumentLink linkedDoc isBroken ->
            let
                (Url url) =
                    linkResolver linkedDoc
            in
            a [ href url ] [ Html.text (toString linkedDoc.slug) ]

        WebLink (Url url) ->
            a [ href url ] [ Html.text url ]


linkAsHtmlWith : (DocumentReference -> Url) -> Link -> List (Html msg) -> Html msg
linkAsHtmlWith linkResolver link childs =
    case link of
        DocumentLink linkedDoc isBroken ->
            let
                (Url url) =
                    linkResolver linkedDoc
            in
            a [ href url ] childs

        WebLink (Url url) ->
            a [ href url ] childs


sliceAsHtml : (DocumentReference -> Url) -> Slice -> Html msg
sliceAsHtml linkResolver slice =
    fieldAsHtml linkResolver slice.sliceField


{-| Provide a default URL for `linkedDocuments`:

    Url "documents/doc.id/doc.slug"

-}
defaultLinkResolver : DocumentReference -> Url
defaultLinkResolver linkedDoc =
    Url (String.join "/" [ "documents", linkedDoc.id, linkedDoc.slug ])


viewDefaultDocType : Document -> Html msg
viewDefaultDocType (Document doc) =
    div []
        ([ h2 [] (List.map Html.text (Dict.keys doc)) ]
            ++ List.map (fieldAsHtml defaultLinkResolver) (Dict.values doc)
        )


{-| Get the first title out of some `StructuredText`, if there is one.
-}
getTitle : StructuredText -> Maybe StructuredTextBlock
getTitle structuredText =
    let
        isTitle field =
            case field of
                Heading1 _ ->
                    True

                Heading2 _ ->
                    True

                Heading3 _ ->
                    True

                _ ->
                    False
    in
    List.head (List.filter isTitle structuredText)


{-| Get the first paragraph out of some `StructuredText`, if there is one.
-}
getFirstParagraph : StructuredText -> Maybe StructuredTextBlock
getFirstParagraph structuredText =
    let
        isParagraph field =
            case field of
                Paragraph _ ->
                    True

                _ ->
                    False
    in
    List.head (List.filter isParagraph structuredText)


{-| Get the first image out of some `StructuredText`, if there is one.
-}
getFirstImage : StructuredText -> Maybe ImageView
getFirstImage structuredText =
    let
        getImage field =
            case field of
                SImage image ->
                    Just image

                _ ->
                    Nothing
    in
    List.head (List.filterMap getImage structuredText)


{-| Get the contents of a single `StructuredText` element as a `String`.
-}
getText : StructuredTextBlock -> String
getText field =
    case field of
        Heading1 block ->
            block.text

        Heading2 block ->
            block.text

        Heading3 block ->
            block.text

        Paragraph block ->
            block.text

        ListItem block ->
            block.text

        SImage imageField ->
            Maybe.withDefault "<image>" imageField.alt

        SEmbed _ ->
            ""


{-| Get the contents of a some `StructuredText` as a `String`.
-}
getTexts : StructuredText -> String
getTexts fields =
    fields
        |> List.map getText
        |> String.join " "



-- INTERNAL: State


mkUrl : Url -> List ( String, String ) -> Url
mkUrl (Url base) params =
    let
        sep =
            if List.isEmpty params then
                ""
            else
                "?"

        joinParamPair ( key, val ) =
            Http.encodeUri key ++ "=" ++ Http.encodeUri val

        paramsPart =
            params
                |> List.map joinParamPair
                |> String.join "&"
    in
    Url (base ++ sep ++ paramsPart)


requestToUrl : Request -> Url
requestToUrl request =
    let
        (Ref refStr) =
            request.ref
    in
    mkUrl request.action
        (( "ref", refStr )
            :: (if String.isEmpty request.q then
                    []
                else
                    [ ( "q", request.q ) ]
               )
        )


getRefById : String -> Api -> Maybe RefProperties
getRefById refId api =
    api.refs
        |> List.filter (\r -> r.id == refId)
        |> List.head


predicatesToStr : List Predicate -> String
predicatesToStr predicates =
    let
        wrapQuotes value =
            "\"" ++ value ++ "\""

        toStrList values =
            let
                valueStrs =
                    values
                        |> List.map wrapQuotes
                        |> String.join ", "
            in
            "[" ++ valueStrs ++ "]"

        predicateToStr predicate =
            let
                query =
                    case predicate of
                        At fragment value ->
                            "at(" ++ fragment ++ ", " ++ wrapQuotes value ++ ")"

                        AtL fragment values ->
                            "at(" ++ fragment ++ ", " ++ toStrList values ++ ")"

                        Any fragment values ->
                            "any(" ++ fragment ++ ", " ++ toStrList values ++ ")"

                        FullText fragment value ->
                            "fulltext(" ++ fragment ++ ", " ++ wrapQuotes value ++ ")"
            in
            "[:d = " ++ query ++ "]"
    in
    "[" ++ String.concat (List.map predicateToStr predicates) ++ "]"


getFromCache :
    Request
    -> Model_ api
    -> Maybe (Response Document)
getFromCache request prismic =
    Dict.get (requestToKey request) prismic.cache


setInCache :
    Request
    -> Response Document
    -> Model_ api
    -> Model_ api
setInCache request response prismic =
    { prismic
        | cache = Dict.insert (requestToKey request) response prismic.cache
    }


requestToKey : Request -> String
requestToKey =
    toString
