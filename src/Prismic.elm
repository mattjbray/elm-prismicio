module Prismic exposing (init, fetchApi, form, ref, query, bookmark, none, submit, any, at, atL, fulltext)

{-| An Elm SDK for [Prismic.io](https://prismic.io).

# Initialisation
@docs init

# Making a request
@docs fetchApi, form, submit

# Customising the request
@docs ref, bookmark, query, none

# Predicates
@docs at, atL, any, fulltext
-}

import Dict
import Json.Decode as Json exposing (Decoder)
import Http
import Task exposing (Task)
import Prismic.Types exposing (..)
import Prismic.Decoders exposing (..)
import Prismic.Internal.State exposing (..)


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
    , requests = Dict.empty
    , responses = Dict.empty
    }


{-| Go and fetch the Prismic API, if it has not already been fetched. You must
start every Prismic request with this function.
-}
fetchApi : Model -> Task PrismicError ModelWithApi
fetchApi cache =
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
                        (getJson decodeApi url)
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
                                        `Maybe.andThen` .default
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
        apiTask `Task.andThen` addForm


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
        requestTask `Task.andThen` addRef


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
        requestTask `Task.andThen` addQuery


{-| Convenience function for fetching a bookmarked document.
-}
bookmark :
    String
    -> Task PrismicError (ModelWithApi)
    -> Task PrismicError ( Request, ModelWithApi )
bookmark bookmarkId cacheTask =
    cacheTask
        `Task.andThen` (\cacheWithApi ->
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


{-| Pass the request through unmodified.

Useful for conditionally adding a query.
-}
none :
    Task PrismicError ( Request, Model' api )
    -> Task PrismicError ( Request, Model' api )
none =
    Task.map identity


{-| Submit the request.

Pass this function a Json `Decoder` to decode each document in the response into
your own Elm type, or use `decodeDefaultDocType`.
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

                decodeResponseValue responseValue =
                    Json.decodeValue (decodeResponse decodeDocType) responseValue
                        |> Task.fromResult
                        |> Task.mapError (\msg -> SubmitRequestError (Http.UnexpectedPayload msg))
            in
                case getFromCache request cache of
                    Just responseValue ->
                        decodeResponseValue responseValue
                            |> Task.map (\response -> ( response, cacheWithApi ))

                    Nothing ->
                        let
                            fetchUrl =
                                getJson Json.value url
                                    |> Task.mapError SubmitRequestError

                            decodeAndMkResult responseValue =
                                decodeResponseValue responseValue
                                    |> Task.map (mkResultTuple responseValue)

                            mkResultTuple responseValue response =
                                ( response
                                , setInCache request responseValue cacheWithApi
                                )
                        in
                            fetchUrl `Task.andThen` decodeAndMkResult
    in
        requestTask `Task.andThen` doSubmit



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
