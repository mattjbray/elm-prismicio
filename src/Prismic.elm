module Prismic
    exposing
        ( Model
        , ModelWithApi
        , Predicate
        , PrismicError(..)
        , Request
        , any
        , api
        , at
        , atL
        , bookmark
        , cache
        , form
        , fulltext
        , getApi
        , init
        , query
        , ref
        , submit
        )

{-| An Elm SDK for [Prismic.io](https://prismic.io).


# Initialisation

@docs init


# Initiating a request

@docs Request, api, form, bookmark


# Customising the request

@docs ref, query


# Sending the request

@docs submit, cache


# Predicates

@docs Predicate, at, atL, any, fulltext


# Types


## Models

@docs Model, ModelWithApi, getApi


## Errors

@docs PrismicError

-}

import Dict exposing (Dict)
import Http
import Prismic.Api exposing (Response, Api, Ref(Ref), RefProperties, decodeApi, decodeResponse)
import Prismic.Document as Document exposing (Document)
import Prismic.Url exposing (Url(Url), withQuery)
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
    , url : Url
    , nextRequestId : Int
    , cache : Dict String (Response Document.Document)
    }


{-| Get the `Api` information from the `Model`.
-}
getApi : ModelWithApi -> Api
getApi (ModelWithApi model) =
    model.api



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



-- REQUEST


{-| Represents a Prismic request.
-}
type Request
    = Request
        { action : Url
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
            Prismic.init (Url "https://lesbonneschoses.prismic.io/api")
        }

-}
init : Url -> Model
init url =
    Model
        { api = Nothing
        , url = url
        , nextRequestId = 0
        , cache = Dict.empty
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
            let
                (Url url) =
                    cache.url
            in
                Task.map (\api -> ModelWithApi { cache | api = api })
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
        addForm (ModelWithApi cache) =
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
                                ( Request
                                    { action = form.action
                                    , ref = masterRef.ref
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
                (Url url) =
                    requestToUrl request

                cacheWithApi =
                    { cache | api = Just cache.api }

                decodeResponseToUserDocType response =
                    response.results
                        |> List.map
                            (\result ->
                                Document.decodeDocument decodeDocType result.data
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
                            |> Task.map (\response -> ( response, Model cacheWithApi ))

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


requestToUrl : Request -> Url
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
