module Prismic
    exposing
        ( Api
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
        , any
        , api
        , at
        , atL
        , bookmark
        , cache
        , defaultOptions
        , form
        , fulltext
        , getApi
        , init
        , initWith
        , query
        , ref
        , submit
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

-}

import Dict exposing (Dict)
import Http
import Json.Decode as Json
import Json.Decode.Pipeline as Json exposing (custom, optional, required, requiredAt)
import Prismic.Document as Document exposing (Document)
import Prismic.Document.Internal as Internal
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
    , cache : Dict String (Response Document.Document)
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
        |> required "results" (Json.list Internal.decodeSearchResult)
        |> required "results_per_page" Json.int
        |> required "results_size" Json.int
        |> required "total_pages" Json.int
        |> required "total_results_size" Json.int
        |> required "version" Json.string



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
    Document.Decoder docType
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
