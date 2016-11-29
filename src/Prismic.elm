module Prismic
    exposing
        ( init
        , api
        , form
        , bookmark
        , ref
        , query
        , none
        , submit
        , collectResponses
        , any
        , at
        , atL
        , fulltext
        , Url(Url)
        , Model
        , ModelWithApi
        , Model_
        , PrismicError(..)
        , Api
        , RefProperties
        , Ref(Ref)
        , Form
        , FormField
        , FieldType
        , Experiments
        , Predicate
        , Request
        , Response
        , DefaultDocType
        , DocumentField(..)
        , decodeDefaultDocType
        , StructuredText
        , StructuredTextBlock(..)
        , Block
        , Span
        , SpanElement(..)
        , ImageViews
        , ImageView
        , ImageDimensions
        , Embed(..)
        , EmbedRich
        , EmbedVideo
        , Link(DocumentLink, WebLink)
        , DocumentReference
        , decodeStructuredText
        , decodeImageViews
        , decodeLink
        , decodeEmbed
        , structuredTextAsHtml
        , defaultLinkResolver
        , getFirstImage
        , getFirstParagraph
        , getText
        , getTexts
        , getTitle
        )

{-|
An Elm SDK for [Prismic.io](https://prismic.io).

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

### Default document
@docs DefaultDocType, DocumentField, decodeDefaultDocType

### Custom documents

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

### Custom document decoders

@docs decodeStructuredText
@docs decodeImageViews
@docs decodeLink
@docs decodeEmbed

## Viewing documents
@docs structuredTextAsHtml
@docs defaultLinkResolver

### `StructuredText` helpers
@docs getTitle, getFirstImage, getFirstParagraph, getText, getTexts
-}

import Dict exposing (Dict)
import Json.Decode as Json
import Json.Decode.Pipeline exposing (decode, custom, required, requiredAt, optional)
import Json.Encode
import Http
import Html exposing (..)
import Html.Attributes exposing (href, property, src)
import Task exposing (Task)
import Task.Extra as Task
import String


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
    , cache : Dict String Json.Value
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


{-| TODO: Experiments are not Strings.  Fill out this type.
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


{-| A default document type.

Normally you will want to define your own document types and decoders.
-}
type alias DefaultDocType =
    Dict String (Dict String (List DocumentField))


{-| A field in the `DefaultDocType`.
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

Pass this function a `Json.Decoder` to decode each document in the response into
your own Elm type, or use `decodeDefaultDocType`.
-}
submit :
    Json.Decoder docType
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

                fakeResponse =
                    { url = ""
                    , status = { code = 200, message = "OK" }
                    , headers = Dict.empty
                    , body = ""
                    }

                decodeResponseValue responseValue =
                    Json.decodeValue (decodeResponse decodeDocType) responseValue
                        |> Task.fromResult
                        |> Task.mapError (\msg -> SubmitRequestError (Http.BadPayload msg fakeResponse))
            in
                case getFromCache request cache of
                    Just responseValue ->
                        decodeResponseValue responseValue
                            |> Task.map (\response -> ( response, cacheWithApi ))

                    Nothing ->
                        let
                            fetchUrl =
                                Http.get url Json.value
                                    |> Http.toTask
                                    |> Task.mapError SubmitRequestError

                            decodeAndMkResult responseValue =
                                decodeResponseValue responseValue
                                    |> Task.map (mkResultTuple responseValue)

                            mkResultTuple responseValue response =
                                ( response
                                , setInCache request responseValue cacheWithApi
                                )
                        in
                            fetchUrl |> Task.andThen decodeAndMkResult
    in
        requestTask |> Task.andThen doSubmit


{-| The `submit` `Task` returns an updated Prismic `Model` with the request and
response cached.

In your app's `update` function, you should merge this with the existing cache
using `collectResponses`.
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
    Json.maybe decoder |> Json.andThen (Json.succeed << (Maybe.withDefault default))


decodeRef : Json.Decoder Ref
decodeRef =
    Json.map Ref Json.string


decodeUrl : Json.Decoder Url
decodeUrl =
    Json.map Url Json.string



-- DECODERS


decodeApi : Json.Decoder Api
decodeApi =
    decode Api
        |> required "refs" (Json.list decodeRefProperties)
        |> required "bookmarks" (Json.dict Json.string)
        |> required "types" (Json.dict Json.string)
        |> required "tags" (Json.list Json.string)
        |> required "version" (Json.string)
        |> required "forms" (Json.dict decodeForm)
        |> required "oauth_initiate" (Json.string)
        |> required "oauth_token" (Json.string)
        |> required "license" (Json.string)
        |> required "experiments" (decodeExperiments)


decodeRefProperties : Json.Decoder RefProperties
decodeRefProperties =
    decode RefProperties
        |> required "id" Json.string
        |> required "ref" decodeRef
        |> required "label" Json.string
        |> optional "isMasterRef" Json.bool False


decodeForm : Json.Decoder Form
decodeForm =
    decode Form
        |> required "method" (Json.string)
        |> required "enctype" (Json.string)
        |> required "action" (decodeUrl)
        |> required "fields" (Json.dict decodeFormField)
        |> optional "rel" (Json.maybe Json.string) Nothing
        |> optional "name" (Json.maybe Json.string) Nothing


decodeFormField : Json.Decoder FormField
decodeFormField =
    decode FormField
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
    decode Experiments
        |> required "draft" (Json.list Json.string)
        |> required "running" (Json.list Json.string)


decodeResponse : Json.Decoder docType -> Json.Decoder (Response docType)
decodeResponse decodeDocType =
    decode Response
        |> required "license" (Json.string)
        |> required "next_page" (Json.nullable decodeUrl)
        |> required "page" (Json.int)
        |> required "prev_page" (Json.nullable decodeUrl)
        |> required "results" (Json.list (decodeSearchResult decodeDocType))
        |> required "results_per_page" (Json.int)
        |> required "results_size" (Json.int)
        |> required "total_pages" (Json.int)
        |> required "total_results_size" (Json.int)
        |> required "version" (Json.string)


{-| Decode a result to a `DefaultDocType`.
-}
decodeDefaultDocType : Json.Decoder DefaultDocType
decodeDefaultDocType =
    Json.field "data"
        (Json.dict
            (Json.dict
                (Json.oneOf
                    [ Json.map (\x -> [ x ]) decodeDocumentField
                    , Json.list decodeDocumentField
                    ]
                )
            )
        )


decodeSearchResult : Json.Decoder docType -> Json.Decoder (SearchResult docType)
decodeSearchResult decodeDocType =
    decode SearchResult
        |> custom decodeDocType
        |> required "href" (decodeUrl)
        |> required "id" (Json.string)
        |> required "linked_documents" (Json.list decodeDocumentReference)
        |> required "slugs" (Json.list Json.string)
        |> required "tags" (Json.list Json.string)
        |> required "type" (Json.string)
        |> required "uid" (Json.nullable Json.string)


decodeDocumentReference : Json.Decoder DocumentReference
decodeDocumentReference =
    decode DocumentReference
        |> required "id" (Json.string)
        |> required "slug" (Json.string)
        |> required "tags" (Json.list Json.string)
        |> required "type" (Json.string)


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

                _ ->
                    Json.fail ("Unknown document field type: " ++ typeStr)
    in
        (Json.field "type" Json.string) |> Json.andThen decodeOnType


{-| Decode some `StructuredText`.
-}
decodeStructuredText : Json.Decoder StructuredText
decodeStructuredText =
    Json.list decodeStructuredTextBlock


{-| Decode an `ImageField`.
-}
decodeImageViews : Json.Decoder ImageViews
decodeImageViews =
    decode ImageViews
        |> required "main" decodeImageView
        |> required "views" (Json.dict decodeImageView)


decodeImageView : Json.Decoder ImageView
decodeImageView =
    decode ImageView
        |> required "alt" (Json.nullable Json.string)
        |> required "copyright" (Json.nullable Json.string)
        |> required "url" (decodeUrl)
        |> required "dimensions" (decodeImageDimensions)


decodeImageDimensions : Json.Decoder ImageDimensions
decodeImageDimensions =
    decode ImageDimensions
        |> required "width" Json.int
        |> required "height" Json.int


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
    decode Block
        |> required "text" Json.string
        |> required "spans" (Json.list decodeSpan)


decodeSpan : Json.Decoder Span
decodeSpan =
    decode Span
        |> required "start" Json.int
        |> required "end" Json.int
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
    decode EmbedVideo
        |> required "author_name" Json.string
        |> required "author_url" decodeUrl
        |> required "embed_url" decodeUrl
        |> required "height" Json.int
        |> required "html" Json.string
        |> required "provider_name" Json.string
        |> required "provider_url" decodeUrl
        |> required "thumbnail_height" Json.int
        |> required "thumbnail_url" decodeUrl
        |> required "thumbnail_width" Json.int
        |> required "title" Json.string
        |> required "version" Json.string
        |> required "width" Json.int


decodeEmbedRich : Json.Decoder EmbedRich
decodeEmbedRich =
    decode EmbedRich
        |> required "author_name" Json.string
        |> required "author_url" decodeUrl
        |> required "cache_age" Json.string
        |> required "embed_url" decodeUrl
        |> required "height" (Json.maybe Json.int)
        |> required "html" Json.string
        |> required "provider_name" Json.string
        |> required "provider_url" decodeUrl
        |> required "title" Json.string
        |> required "url" decodeUrl
        |> required "version" Json.string
        |> required "width" Json.int


{-| Decode a `Link`.
-}
decodeLink : Json.Decoder Link
decodeLink =
    let
        decodeOnType typeStr =
            case typeStr of
                "Link.document" ->
                    decode DocumentLink
                        |> requiredAt [ "value", "document" ] decodeDocumentReference
                        |> requiredAt [ "value", "isBroken" ] Json.bool

                "Link.web" ->
                    decode WebLink
                        |> requiredAt [ "value", "url" ] decodeUrl

                _ ->
                    Json.fail ("Unknown link type: " ++ typeStr)
    in
        Json.field "type" Json.string |> Json.andThen decodeOnType



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
                                asHtml linkResolver doc

                            _ ->
                                div [] (List.map (asHtml linkResolver) docs)
                        )
                )
        )


asHtml : (DocumentReference -> Url) -> DocumentField -> Html msg
asHtml linkResolver field =
    case field of
        Text t ->
            span [] [ text t ]

        Date t ->
            span [] [ text t ]

        Number n ->
            span [] [ text (toString n) ]

        Select t ->
            span [] [ text t ]

        Color t ->
            span [] [ text ("<Color> " ++ t) ]

        Link l ->
            linkAsHtml linkResolver l

        Image i ->
            imageAsHtml i.main

        StructuredText fields ->
            div [] (structuredTextAsHtml linkResolver fields)


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
                ( childs ++ [ text beginning, (spanEl span) [ text middle ] ]
                , span.end
                )
    in
        el []
            (field.spans
                |> List.sortBy .start
                |> List.foldl foldFn ( [], 0 )
                |> (\( childs, index ) -> childs ++ [ text (String.dropLeft index field.text) ])
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
                a [ href url ] [ text (toString linkedDoc.slug) ]

        WebLink (Url url) ->
            a [ href url ] [ text url ]


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


{-| Provide a default URL for `linkedDocuments`:

    Url "documents/doc.id/doc.slug"
-}
defaultLinkResolver : DocumentReference -> Url
defaultLinkResolver linkedDoc =
    Url (String.join "/" [ "documents", linkedDoc.id, linkedDoc.slug ])


viewDefaultDocType : DefaultDocType -> Html msg
viewDefaultDocType doc =
    let
        allDocFields =
            let
                fieldsPerType =
                    Dict.values doc

                fieldsPerField =
                    List.concatMap Dict.values fieldsPerType
            in
                List.concat fieldsPerField
    in
        div []
            ([ h2 [] (List.map text (Dict.keys doc)) ]
                ++ List.map (asHtml defaultLinkResolver) allDocFields
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
                :: if String.isEmpty request.q then
                    []
                   else
                    [ ( "q", request.q ) ]
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
    -> Maybe Json.Value
getFromCache request prismic =
    Dict.get (requestToKey request) prismic.cache


setInCache :
    Request
    -> Json.Value
    -> Model_ api
    -> Model_ api
setInCache request response prismic =
    { prismic
        | cache = Dict.insert (requestToKey request) response prismic.cache
    }


requestToKey : Request -> String
requestToKey =
    toString
