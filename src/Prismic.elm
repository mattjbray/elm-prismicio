module Prismic exposing
    ( init, initWith, Options, defaultOptions
    , Request, api, form, bookmark
    , ref, lang, query
    , submit
    , Response, cache
    , Predicate, at, atL, any, fulltext
    , Model
    , PrismicError(..)
    , Api, RefProperties, Ref, Form, FormField, FieldType, Experiments
    , Decoder
    , succeed, fail, map, apply, andThen
    , decode, custom
    , Document
    , id, href, linkedDocuments, slugs, tags, uid
    , requiredField, optionalField
    , groupField, sliceZoneField
    , required, optional
    , group, sliceZone
    )

{-| An Elm SDK for [Prismic.io](https://prismic.io).


# Initialisation

@docs init, initWith, Options, defaultOptions


# Initiating a request

@docs Request, api, form, bookmark


# Customising the request

@docs ref, lang, query


# Sending the request

@docs submit


# Handle the response

@docs Response, cache


# Predicates

@docs Predicate, at, atL, any, fulltext


# Types


## Models

@docs Model


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

@docs requiredField, optionalField
@docs groupField, sliceZoneField


### Pipeline decoders

@docs required, optional
@docs group, sliceZone

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
import Url


{-| The Prismic Model keeps track of configuration and holds the response cache.
-}
type Model
    = Model
        { api : Maybe Api
        , url : String
        , nextRequestId : Int
        , cache : Dict String (Response Document)
        , options : Options
        }



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


updateResults : Response a -> List b -> Response b
updateResults response results =
    { license = response.license
    , nextPage = response.nextPage
    , page = response.page
    , prevPage = response.prevPage
    , results = results
    , resultsPerPage = response.resultsPerPage
    , resultsSize = response.resultsSize
    , totalPages = response.totalPages
    , totalResultsSize = response.totalResultsSize
    , version = response.version
    }



-- DECODERS


decodeRef : Json.Decoder Ref
decodeRef =
    Json.map Ref Json.string


{-| Decode an `Api` from JSON.
-}
decodeApi : Json.Decoder Api
decodeApi =
    Json.succeed Api
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
    Json.succeed RefProperties
        |> Json.required "id" Json.string
        |> Json.required "ref" decodeRef
        |> Json.required "label" Json.string
        |> Json.optional "isMasterRef" Json.bool False


decodeForm : Json.Decoder Form
decodeForm =
    Json.succeed Form
        |> Json.required "method" Json.string
        |> Json.required "enctype" Json.string
        |> Json.required "action" Json.string
        |> Json.required "fields" (Json.dict decodeFormField)
        |> Json.optional "rel" (Json.maybe Json.string) Nothing
        |> Json.optional "name" (Json.maybe Json.string) Nothing


decodeFormField : Json.Decoder FormField
decodeFormField =
    Json.succeed FormField
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
    Json.succeed Experiments
        |> Json.required "draft" (Json.list Json.string)
        |> Json.required "running" (Json.list Json.string)


{-| Decode a `Response` from JSON.
-}
decodeResponse : Json.Decoder (Response Document)
decodeResponse =
    Json.succeed Response
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
        { model : Model
        , api : Api
        , config : RequestConfig
        }


type alias RequestConfig =
    { action : String
    , ref : Ref
    , q : String
    , lang : String
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


{-| Go and fetch the Prismic `Api` metadata, if it has not already been fetched.

The `Api` is cached in the returned `Model`, so we don't have to fetch it next
time.

You start every Prismic request with this function.

-}
api : Model -> Task PrismicError ( Model, Api )
api (Model model) =
    case model.api of
        Just api_ ->
            Task.succeed ( Model model, api_ )

        Nothing ->
            { method = "GET"
            , headers = []
            , url = model.url
            , body = Http.emptyBody
            , resolver = Http.stringResolver <| handleJsonResponse <| decodeApi
            , timeout = Nothing
            }
                |> Http.task
                |> Task.mapError FetchApiError
                |> Task.map
                    (\api_ ->
                        ( Model { model | api = Just api_ }
                        , api_
                        )
                    )


{-| Choose a form on which to base the rest of the Prismic request.
-}
form :
    String
    -> Task PrismicError ( Model, Api )
    -> Task PrismicError Request
form formId apiTask =
    let
        addForm ( Model model, api_ ) =
            let
                mForm =
                    Dict.get formId api_.forms

                ref_ =
                    getRefById model.options.defaultRef api_
                        |> Maybe.map .ref
                        |> Maybe.withDefault (Ref model.options.defaultRef)
            in
            case mForm of
                Nothing ->
                    Task.fail (FormDoesNotExist formId)

                Just form_ ->
                    let
                        q =
                            Maybe.withDefault ""
                                (Dict.get "q" form_.fields
                                    |> Maybe.andThen .default
                                )
                    in
                    Task.succeed
                        (Request
                            { api = api_
                            , model = Model model
                            , config =
                                { action = form_.action
                                , ref = ref_
                                , q = q
                                , lang = ""
                                }
                            }
                        )
    in
    apiTask |> Task.andThen addForm


{-| Convenience function for fetching a bookmarked document.
-}
bookmark :
    String
    -> Task PrismicError ( Model, Api )
    -> Task PrismicError Request
bookmark bookmarkId cacheTask =
    cacheTask
        |> Task.andThen
            (\( model, api_ ) ->
                let
                    mDocId =
                        Dict.get bookmarkId api_.bookmarks
                in
                case mDocId of
                    Nothing ->
                        Task.fail (BookmarkDoesNotExist bookmarkId)

                    Just docId ->
                        Task.succeed ( model, api_ )
                            |> form "everything"
                            |> query [ at "document.id" docId ]
            )


{-| Override a Form's default ref
-}
ref :
    String
    -> Task PrismicError Request
    -> Task PrismicError Request
ref refId requestTask =
    let
        setRef ref_ (Request request) =
            let
                config =
                    request.config
            in
            Request
                { request
                    | config = { config | ref = ref_.ref }
                }

        addRef (Request request) =
            case getRefById refId request.api of
                Nothing ->
                    Task.fail (RefDoesNotExist refId)

                Just r ->
                    Request request
                        |> setRef r
                        |> Task.succeed
    in
    requestTask |> Task.andThen addRef


{-| Override a Form's default lang.
-}
lang :
    String
    -> Task PrismicError Request
    -> Task PrismicError Request
lang lang_ requestTask =
    let
        setLang (Request request) =
            let
                config =
                    request.config
            in
            Request
                { request
                    | config = { config | lang = lang_ }
                }

        addLang request =
            request
                |> setLang
                |> Task.succeed
    in
    requestTask |> Task.andThen addLang


{-| Override a Form's default query.

See the section on `Predicate`s below for how to construct a `Predicate`.

-}
query :
    List Predicate
    -> Task PrismicError Request
    -> Task PrismicError Request
query predicates requestTask =
    let
        setQuery query_ (Request request) =
            let
                config =
                    request.config
            in
            Request
                { request
                    | config = { config | q = query_ }
                }

        addQuery request =
            request
                |> setQuery (predicatesToStr predicates)
                |> Task.succeed
    in
    requestTask |> Task.andThen addQuery


handleJsonResponse : Json.Decoder doc -> Http.Response String -> Result Http.Error doc
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Json.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


{-| Submit the request.

Pass this function a `Decoder` to decode each document in the response into your
own Elm document type.

-}
submit :
    Decoder Document docType
    -> Task PrismicError Request
    -> Task PrismicError ( Model, Response docType )
submit decodeDocType requestTask =
    let
        doSubmit (Request request) =
            let
                decodeResponseToUserDocType response =
                    response.results
                        |> List.map (Internal.decodeValue decodeDocType)
                        |> Result.collect
                        |> Result.map (updateResults response)
                        |> Task.fromResult
                        |> Task.mapError DecodeDocumentError
            in
            case getFromCache request.config request.model of
                Just response ->
                    decodeResponseToUserDocType response
                        |> Task.map (Tuple.pair request.model)

                Nothing ->
                    { method = "GET"
                    , headers = []
                    , url = requestToUrl request.config
                    , body = Http.emptyBody
                    , resolver = Http.stringResolver <| handleJsonResponse <| decodeResponse
                    , timeout = Nothing
                    }
                        |> Http.task
                        |> Task.mapError SubmitRequestError
                        |> Task.andThen
                            (\origResponse ->
                                decodeResponseToUserDocType origResponse
                                    |> Task.map
                                        (\response ->
                                            ( setInCache request.config origResponse request.model
                                            , response
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
            MyPrismicMsg (Ok ( prismic, response )) ->
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


{-| `at fragment value` matches documents having `value` at `fragment`.
-}
at : String -> String -> Predicate
at fragment value =
    At fragment value


{-| `atL fragment values` matches documents having a list of `values` at `fragment`.
-}
atL : String -> List String -> Predicate
atL fragment values =
    AtL fragment values


{-| `any fragment values` matches documents having any of `values` at `fragment`.
-}
any : String -> List String -> Predicate
any fragment values =
    Any fragment values


{-| `fulltext fragment value` matches documents with a full text search at `fragment`.
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
            Url.percentEncode key ++ "=" ++ Url.percentEncode val

        paramsPart =
            params
                |> List.map joinParamPair
                |> String.join "&"
    in
    base ++ sep ++ paramsPart


requestToUrl : RequestConfig -> String
requestToUrl config =
    let
        (Ref refStr) =
            config.ref

        ifNotEmpty key val =
            if String.isEmpty val then
                []

            else
                [ ( key, val ) ]
    in
    config.action
        |> withQuery
            (List.concat
                [ [ ( "ref", refStr ) ]
                , ifNotEmpty "q" config.q
                , ifNotEmpty "lang" config.lang
                ]
            )


getRefById : String -> Api -> Maybe RefProperties
getRefById refId api_ =
    api_.refs
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
                query_ =
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
            "[:d = " ++ query_ ++ "]"
    in
    "[" ++ String.concat (List.map predicateToStr predicates) ++ "]"


getFromCache :
    RequestConfig
    -> Model
    -> Maybe (Response Document)
getFromCache request (Model model) =
    Dict.get (requestToKey request) model.cache


setInCache :
    RequestConfig
    -> Response Document
    -> Model
    -> Model
setInCache request response (Model model) =
    Model
        { model
            | cache =
                Dict.insert (requestToKey request) response model.cache
        }


requestToKey : RequestConfig -> String
requestToKey requestConfig =
    let
        (Ref ref_) =
            requestConfig.ref
    in
    String.join ":::"
        [ requestConfig.action
        , ref_
        , requestConfig.q
        , requestConfig.lang
        ]



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

The following is equivalent to the example using `required`:

    myDocDecoder : Decoder Document MyDoc
    myDocDecoder =
        decode MyDoc
            |> custom (requiredField "title" structuredText)

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
requiredField : String -> Decoder Field a -> Decoder Document a
requiredField =
    Internal.requiredField getKey


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
        Prismic.map MyDoc
            (Prismic.groupField "albums" albumDecoder)

-}
groupField : String -> Decoder Group a -> Decoder Document (List a)
groupField key decoder =
    Internal.Decoder
        (\doc ->
            case Dict.get key doc.data of
                Just (Internal.Groups groups) ->
                    groups
                        |> List.map (Internal.decodeValue decoder)
                        |> Result.collect

                Just field ->
                    Err ("Expected a Group field, but got '" ++ Internal.documentFieldTypeToString field ++ "'.")

                Nothing ->
                    Ok []
        )


{-| Pipeline version of `groupField`.
-}
group : String -> Decoder Group a -> Decoder Document (List a -> b) -> Decoder Document b
group key decoder =
    custom (groupField key decoder)


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
        Prismic.map MyDoc
            (Prismic.sliceZoneField "sections" sectionDecoder)

    sectionDecoder : Decoder Slice Section
    sectionDecoder =
        Slice.oneOf
            [ Slice.slice "my-content"
                -- Decode the non-repeating zone and ignore the repeating zone.
                (Group.field "text" Field.structuredText)
                (Prismic.succeed ())
                |> Prismic.map (\( content, _ ) -> MyContent content)
            , Slice.slice "my-image-gallery"
                -- Ignore the non-repeating zone and decode the repeating zone.
                (Prismic.succeed ())
                (Group.field "image" Field.image)
                |> Prismic.map (\( _, images ) -> MyImageGallery images)
            , Slice.slice "my-links-section"
                -- Decode both the non-repeating and repeating zones.
                (Group.field "title" Field.structuredText)
                (Group.field "link" Field.link)
                |> Prismic.map
                    (\( title, links ) -> MyLinksSection (LinksSection title links))
            ]

-}
sliceZoneField : String -> Decoder Slice a -> Decoder Document (List a)
sliceZoneField key sliceDecoder =
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


{-| Pipeline version of `sliceZoneField`.
-}
sliceZone : String -> Decoder Slice a -> Decoder Document (List a -> b) -> Decoder Document b
sliceZone key sliceDecoder =
    custom (sliceZoneField key sliceDecoder)
