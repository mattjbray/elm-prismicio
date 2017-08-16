module Prismic
    exposing
        ( Api
        , Decoder
        , Document
        , Experiments
        , FieldType
        , Form
        , FormField
        , Model
        , ModelWithApi
        , Options
        , Predicate
        , PrismicError(..)
        , Ref
        , RefProperties
        , Request
        , Response
        , andThen
        , any
        , api
        , apply
        , at
        , atL
        , bookmark
        , cache
        , custom
        , decode
        , defaultOptions
        , fail
        , field
        , form
        , fulltext
        , getApi
        , group
        , href
        , id
        , init
        , initWith
        , linkedDocuments
        , map
        , optional
        , optionalField
        , query
        , ref
        , required
        , sliceZone
        , slugs
        , submit
        , succeed
        , tags
        , uid
        )

{-| An Elm SDK for [Prismic.io](https://prismic.io).


# Initialisation

@docs init, initWith, Options, defaultOptions


# Initiating a request

@docs Request, api, form, bookmark


# Customising the request

@docs ref, query


# Sending the request

@docs submit, Response, cache


# Predicates

@docs Predicate, at, atL, any, fulltext


# Types


## Models

@docs Model, ModelWithApi, getApi


## Errors

@docs PrismicError


## Api

@docs Api, RefProperties, Ref, Form, FormField, FieldType, Experiments


# Decoders

Helpers for decoding various parts of a Document.

@docs Decoder


## Decoder combinators

The following combinators can be used with any `Decoder`.

@docs succeed, fail, map, apply, andThen


## Pipeline decoders

@docs decode, custom


## Decoding documents

@docs Document
@docs id, href, linkedDocuments, slugs, tags, uid


### Decoding custom fields

@docs field, optionalField
@docs group, sliceZone


### Pipeline decoders

@docs required, optional

-}

import Dict exposing (Dict)
import Http
import Json.Decode as Json
import Json.Decode.Pipeline as Json
import Prismic.Field as Field exposing (Field)
import Prismic.Group as Group exposing (Group)
import Prismic.Internal as Internal
import Prismic.Slice as Slice exposing (Slice)
import Result.Extra as Result
import String
import Task exposing (Task)
import Task.Extra as Task


-- Types: Models


{-| This is the main user-facing type for elm-prismicio's internal state.

The `Api` is represented as `Maybe Api`, because we may not have fetched it yet.

-}
type Model
    = Model (Model_ (Maybe Api))


{-| This variation of the Model type is returned by `api`, when we know we have successfully retreived the `Api`.

It is used internally by elm-prismicio.

-}
type ModelWithApi
    = ModelWithApi (Model_ Api)


{-| The generic `Model'` type, where the `Api` is represented by a type parameter.

You will be using the specialised `Model` type in user code.

-}
type alias Model_ api =
    { api : api
    , url : String
    , nextRequestId : Int
    , cache : Dict String (Response Document)
    , options : Options
    }


{-| Get the `Api` information from the `Model`.
-}
getApi : ModelWithApi -> Api
getApi (ModelWithApi model) =
    model.api



-- Types: Options


{-| -}
type alias Options =
    { defaultRef : String }


{-| -}
defaultOptions : Options
defaultOptions =
    { defaultRef = "master" }



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
    , results : List docType
    , resultsPerPage : Int
    , resultsSize : Int
    , totalPages : Int
    , totalResultsSize : Int
    , version : String
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
        |> Json.required "refs" (Json.list decodeRefProperties)
        |> Json.required "bookmarks" (Json.dict Json.string)
        |> Json.required "types" (Json.dict Json.string)
        |> Json.required "tags" (Json.list Json.string)
        |> Json.required "version" Json.string
        |> Json.required "forms" (Json.dict decodeForm)
        |> Json.required "oauth_initiate" Json.string
        |> Json.required "oauth_token" Json.string
        |> Json.required "license" Json.string
        |> Json.required "experiments" decodeExperiments


decodeRefProperties : Json.Decoder RefProperties
decodeRefProperties =
    Json.decode RefProperties
        |> Json.required "id" Json.string
        |> Json.required "ref" decodeRef
        |> Json.required "label" Json.string
        |> Json.optional "isMasterRef" Json.bool False


decodeForm : Json.Decoder Form
decodeForm =
    Json.decode Form
        |> Json.required "method" Json.string
        |> Json.required "enctype" Json.string
        |> Json.required "action" Json.string
        |> Json.required "fields" (Json.dict decodeFormField)
        |> Json.optional "rel" (Json.maybe Json.string) Nothing
        |> Json.optional "name" (Json.maybe Json.string) Nothing


decodeFormField : Json.Decoder FormField
decodeFormField =
    Json.decode FormField
        |> Json.required "type" decodeFieldType
        |> Json.required "multiple" Json.bool
        |> Json.optional "default" (Json.maybe Json.string) Nothing


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
        |> Json.required "draft" (Json.list Json.string)
        |> Json.required "running" (Json.list Json.string)


{-| Decode a `Response` from JSON.
-}
decodeResponse : Json.Decoder (Response Document)
decodeResponse =
    Json.decode Response
        |> Json.required "license" Json.string
        |> Json.required "next_page" (Json.nullable Json.string)
        |> Json.required "page" Json.int
        |> Json.required "prev_page" (Json.nullable Json.string)
        |> Json.required "results" (Json.list Internal.decodeSearchResult)
        |> Json.required "results_per_page" Json.int
        |> Json.required "results_size" Json.int
        |> Json.required "total_pages" Json.int
        |> Json.required "total_results_size" Json.int
        |> Json.required "version" Json.string



-- REQUEST


{-| Represents a Prismic request.
-}
type Request
    = Request
        { action : String
        , ref : Ref
        , q : String
        }



-- FUNCTIONS


{-| Initialise the Prismic model with the URL for your Prismic repository. Save
this in your application's Model somewhere.

    type alias Model =
        { prismic : Prismic.Model }

    init =
        { prismic =
            Prismic.init "https://lesbonneschoses.prismic.io/api"
        }

-}
init : String -> Model
init url =
    initWith url defaultOptions


{-| Initialise with custom options.
-}
initWith : String -> Options -> Model
initWith url options =
    Model
        { api = Nothing
        , url = url
        , nextRequestId = 0
        , cache = Dict.empty
        , options = options
        }


{-| Go and fetch the Prismic API, if it has not already been fetched. You must
start every Prismic request with this function.
-}
api : Model -> Task PrismicError ModelWithApi
api (Model cache) =
    case cache.api of
        Just api ->
            Task.succeed (ModelWithApi { cache | api = api })

        Nothing ->
            Task.map (\api -> ModelWithApi { cache | api = api })
                (Task.mapError FetchApiError
                    (Http.get cache.url decodeApi |> Http.toTask)
                )


{-| Choose a form on which to base the rest of the Prismic request.
-}
form :
    String
    -> Task PrismicError ModelWithApi
    -> Task PrismicError ( Request, ModelWithApi )
form formId apiTask =
    let
        addForm (ModelWithApi cache) =
            let
                mForm =
                    Dict.get formId cache.api.forms

                ref =
                    getRefById cache.options.defaultRef cache.api
                        |> Maybe.map .ref
                        |> Maybe.withDefault (Ref cache.options.defaultRef)
            in
            case mForm of
                Nothing ->
                    Task.fail (FormDoesNotExist formId)

                Just form ->
                    let
                        q =
                            Maybe.withDefault ""
                                (Dict.get "q" form.fields
                                    |> Maybe.andThen .default
                                )
                    in
                    Task.succeed
                        ( Request
                            { action = form.action
                            , ref = ref
                            , q = q
                            }
                        , ModelWithApi cache
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
            (\(ModelWithApi cacheWithApi) ->
                let
                    mDocId =
                        Dict.get bookmarkId cacheWithApi.api.bookmarks
                in
                case mDocId of
                    Nothing ->
                        Task.fail (BookmarkDoesNotExist bookmarkId)

                    Just docId ->
                        Task.succeed (ModelWithApi cacheWithApi)
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
        addRef ( Request request, ModelWithApi cache ) =
            case getRefById refId cache.api of
                Nothing ->
                    Task.fail (RefDoesNotExist refId)

                Just r ->
                    Task.succeed
                        ( Request { request | ref = r.ref }
                        , ModelWithApi cache
                        )
    in
    requestTask |> Task.andThen addRef


{-| Override a Form's default query.

See the section on `Predicate`s below for how to construct a `Predicate`.

-}
query :
    List Predicate
    -> Task PrismicError ( Request, api )
    -> Task PrismicError ( Request, api )
query predicates requestTask =
    let
        addQuery ( Request request, cache ) =
            Task.succeed
                ( Request { request | q = predicatesToStr predicates }
                , cache
                )
    in
    requestTask |> Task.andThen addQuery


{-| Submit the request.

Pass this function a `Decoder` to decode each document in the response into your
own Elm document type.

-}
submit :
    Decoder Document docType
    -> Task PrismicError ( Request, ModelWithApi )
    -> Task PrismicError ( Response docType, Model )
submit decodeDocType requestTask =
    let
        doSubmit ( request, ModelWithApi cache ) =
            let
                cacheWithApi =
                    { cache | api = Just cache.api }

                decodeResponseToUserDocType response =
                    response.results
                        |> List.map (Internal.decodeValue decodeDocType)
                        |> Result.collect
                        |> Result.map (\docs -> { response | results = docs })
                        |> Task.fromResult
                        |> Task.mapError DecodeDocumentError
            in
            case getFromCache request cache of
                Just response ->
                    decodeResponseToUserDocType response
                        |> Task.map (\response -> ( response, Model cacheWithApi ))

                Nothing ->
                    Http.get (requestToUrl request) decodeResponse
                        |> Http.toTask
                        |> Task.mapError SubmitRequestError
                        |> Task.andThen
                            (\origResponse ->
                                decodeResponseToUserDocType origResponse
                                    |> Task.map
                                        (\response ->
                                            ( response
                                            , Model (setInCache request origResponse cacheWithApi)
                                            )
                                        )
                            )
    in
    requestTask |> Task.andThen doSubmit


{-| The `submit` `Task` returns an updated Prismic `Model` with the request and
response cached.

In your app's `update` function, you should merge this with the existing cache
using `cache`.

    update msg model =
        case msg of
            MyPrismicMsg (Ok ( response, prismic )) ->
                { model
                    | prismic =
                        cache model.prismic prismic
                }

-}
cache : Model -> Model -> Model
cache (Model model1) (Model model2) =
    Model
        { model2
            | cache = Dict.union model2.cache model1.cache
        }



-- Predicates


{-| The type representing Prismic query predicates.
-}
type Predicate
    = At String String
    | AtL String (List String)
    | Any String (List String)
    | FullText String String


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



-- INTERNAL: State


withQuery : List ( String, String ) -> String -> String
withQuery params base =
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
    base ++ sep ++ paramsPart


requestToUrl : Request -> String
requestToUrl (Request request) =
    let
        (Ref refStr) =
            request.ref
    in
    request.action
        |> withQuery
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



-- DECODERS


{-| Decoders are parameterized by the input type `val` (`Document`, `Field`,
`Group` or `Slice`) and the result type `a` -- your type representing your
custom Prismic document type.
-}
type alias Decoder val a =
    Internal.Decoder val a


{-| -}
succeed : a -> Decoder val a
succeed =
    Internal.succeed


{-| -}
fail : String -> Decoder val a
fail =
    Internal.fail


{-| Transform a decoder.
-}
map : (a -> b) -> Decoder val a -> Decoder val b
map =
    Internal.map


{-| -}
apply : Decoder val (a -> b) -> Decoder val a -> Decoder val b
apply =
    Internal.apply


{-| -}
andThen : (a -> Decoder val b) -> Decoder val a -> Decoder val b
andThen =
    Internal.andThen



-- Decoding Pipelines


{-| Begin a decoding pipeline.

    type alias MyDoc =
        { title : StructuredText }

    myDocDecoder : Decoder Document MyDoc
    myDocDecoder =
        decode MyDoc
            |> required "title" structuredText

-}
decode : a -> Decoder val a
decode =
    Internal.decode


{-| Use a standard decoder in a pipeline.

The following is equivalent to the example using `required` above:

    myDocDecoder : Decoder Document MyDoc
    myDocDecoder =
        decode MyDoc
            |> custom (field "title" structuredText)

-}
custom : Decoder val a -> Decoder val (a -> b) -> Decoder val b
custom =
    Internal.custom



-- Documents


{-| Holds the Prismic document.

`Documents` consist of basic `Fields`, `Groups` and `Slices`.

You will decode this into your own document type by passing a `Decoder Document
MyDocType` to `submit`.

-}
type alias Document =
    Internal.Document



--  DOCUMENT DECODERS
-- {-| A value that knows how to decode Documents.
-- Construct a `Decoder` to pass to `submit`.
-- -}
-- type alias Decoder a =
--     Internal.Decoder Document a


{-| The document's ID.
-}
id : Decoder Document String
id =
    Internal.Decoder (Ok << .id)


{-| The document's href.
-}
href : Decoder Document String
href =
    Internal.Decoder (Ok << .href)


{-| The document's linked documents.
-}
linkedDocuments : Decoder Document (List Field.DocumentReference)
linkedDocuments =
    Internal.Decoder (Ok << .linkedDocuments)


{-| The document's slugs.
-}
slugs : Decoder Document (List String)
slugs =
    Internal.Decoder (Ok << .slugs)


{-| The document's tags.
-}
tags : Decoder Document (List String)
tags =
    Internal.Decoder (Ok << .tags)


{-| The document's UID.
-}
uid : Decoder Document (Maybe String)
uid =
    Internal.Decoder (Ok << .uid)


getKey : String -> Document -> Maybe (Result String Field)
getKey key doc =
    case Dict.get key doc.data of
        Just (Internal.Field field) ->
            Just (Ok field)

        Just (Internal.SliceZone _) ->
            [ "Expected a Field but got a SliceZone."
            , "(Hint: use sliceZone to decode this.)"
            ]
                |> String.join " "
                |> Err
                |> Just

        Just (Internal.Groups _) ->
            [ "Expected a Field but got a Group."
            , "(Hint: use group to decode this.)"
            ]
                |> String.join " "
                |> Err
                |> Just

        Nothing ->
            Nothing


{-| Decode a field.

Pass this function a `Decoder Field a` from the `Prismic.Field` module.

-}
field : String -> Decoder Field a -> Decoder Document a
field =
    Internal.field getKey


{-| Decode a field that might be missing.
-}
optionalField : String -> Decoder Field a -> a -> Decoder Document a
optionalField =
    Internal.optionalField getKey


{-| Decode a required field.
-}
required : String -> Decoder Field a -> Decoder Document (a -> b) -> Decoder Document b
required =
    Internal.required getKey


{-| Decode a field that might be missing.
-}
optional : String -> Decoder Field a -> a -> Decoder Document (a -> b) -> Decoder Document b
optional =
    Internal.optional getKey


{-| Decode a group.

Pass this function a `Decoder Group a` from the `Prismic.Group` module.

Groups can contain Fields, but not other Groups or Slices.

Here is an example with a document containing a group:

    type alias MyDoc =
        { albums : List Album }

    type alias Album =
        { title : String
        , cover : Field.ImageViews
        }

    albumDecoder : Decoder Group Album
    albumDecoder =
        Prismic.decode Album
            |> Group.required "title" Field.text
            |> Group.required "cover" Field.image

    myDocDecoder : Decoder Document MyDoc
    myDocDecoder =
        Prismic.decode MyDoc
            |> Prismic.custom (Prismic.group "albums" albumDecoder)

-}
group : String -> Decoder Group a -> Decoder Document (List a)
group key decoder =
    Internal.Decoder
        (\doc ->
            case Dict.get key doc.data of
                Just (Internal.Groups groups) ->
                    groups
                        |> List.map (Internal.decodeValue decoder)
                        |> Result.collect

                Just field ->
                    Err ("Expected a Group field, but got '" ++ toString field ++ "'.")

                Nothing ->
                    Ok []
        )


{-| Decode a SliceZone.

Pass this function a `Decoder Slice a` from the `Prismic.Slice` module.

Slices can contain Fields and Groups, but not other Slices.

    type alias MyDoc =
        { sections : List Section }

    type Section
        = -- The "my-content" slice has a non-repeating zone.
          MyContent Field.StructuredText
        | -- The "my-image-gallery" slice has a repeating zone.
          MyImageGallery (List Field.ImageViews)
        | -- The "my-links-section" slice has both non-repeating and repeating
          -- zones.
          MyLinksSection LinksSection

    type alias LinksSection =
        { title : Field.StructuredText
        , links : List Field.Link
        }

    myDocDecoder : Decoder Document MyDoc
    myDocDecoder =
        decode MyDoc
            |> custom (sliceZone "sections" sectionDecoder)

    sectionDecoder : Decoder Slice Section
    sectionDecoder =
        Slice.oneOf
            [ Slice.slice "my-content"
                -- Decode the non-repeating zone and ignore the repeating zone.
                (Group.field "text" Field.structuredText)
                (succeed ())
                |> map (\( content, _ ) -> MyContent content)
            , Slice.slice "my-image-gallery"
                -- Ignore the non-repeating zone and decode the repeating zone.
                (succeed ())
                (Group.field "image" Field.image)
                |> map (\( _, images ) -> MyImageGallery images)
            , Slice.slice "my-links-section"
                -- Decode both the non-repeating and repeating zones.
                (Group.field "title" Field.structuredText)
                (Group.field "link" Field.link)
                |> map
                    (\( title, links ) -> MyLinksSection (LinksSection title links))
            ]

-}
sliceZone : String -> Decoder Slice a -> Decoder Document (List a)
sliceZone key sliceDecoder =
    Internal.Decoder
        (\doc ->
            case Dict.get key doc.data of
                Just (Internal.SliceZone slices) ->
                    slices
                        |> List.map (Internal.decodeValue sliceDecoder)
                        |> Result.collect

                _ ->
                    Err "Expected a SliceZone field."
        )
