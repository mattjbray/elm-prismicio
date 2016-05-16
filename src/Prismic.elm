module Prismic exposing (fetchApi, form, ref, query, submit)

import Dict
import Json.Decode exposing (Decoder)
import Http
import Task exposing (Task)
import Prismic.Types exposing (..)
import Prismic.Decoders exposing (..)
import String


fetchApi : Cache docType -> Task PrismicError (CacheWithApi docType)
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
                        (Http.get decodeApi url)
                    )


form :
    String
    -> Task PrismicError (CacheWithApi docType)
    -> Task PrismicError ( Request, CacheWithApi docType )
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
                case (mForm, mRef) of
                    (Nothing, _) ->
                        Task.fail (FormDoesNotExist formId)

                    (_, Nothing) ->
                        Task.fail (RefDoesNotExist defaultRefId)

                    (Just form, Just masterRef) ->
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


ref :
    String
    -> Task PrismicError ( Request, CacheWithApi docType )
    -> Task PrismicError ( Request, CacheWithApi docType )
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


getRefById : String -> Api -> Maybe RefProperties
getRefById refId api =
    api.refs
        |> List.filter (\r -> r.id == refId)
        |> List.head


query :
    String
    -> Task PrismicError ( Request, CacheWithApi docType )
    -> Task PrismicError ( Request, CacheWithApi docType )
query queryStr requestTask =
    let
        addQuery ( request, cache ) =
            Task.succeed
                ( { request | q = queryStr }
                , cache
                )
    in
        requestTask `Task.andThen` addQuery


submit :
    Decoder docType
    -> Task PrismicError ( Request, CacheWithApi docType )
    -> Task PrismicError ( Response docType, Cache docType )
submit decodeDocType requestTask =
    let
        doSubmit ( request, cache ) =
            let
                (Url url) =
                    requestToUrl request
            in
                case getFromCache request cache of
                    Just response ->
                        Task.succeed ( response, { cache | api = Just cache.api } )

                    Nothing ->
                        let
                            cacheWithApi =
                                { cache | api = Just cache.api }

                            mkResponse response =
                                ( response, setInCache request response cacheWithApi )
                        in
                            Http.get (decodeResponse decodeDocType) url
                                |> Task.map mkResponse
                                |> Task.mapError SubmitRequestError
    in
        requestTask `Task.andThen` doSubmit


requestToUrl : Request -> Url
requestToUrl request =
    let
        (Ref refStr) =
            request.ref

        (Url urlStr) =
            request.action
    in
        Url
            (Http.url urlStr
                (( "ref", refStr )
                    :: if String.isEmpty request.q then
                        []
                       else
                        [ ( "q", request.q ) ]
                )
            )


getFromCache :
    Request
    -> Cache' api docType
    -> Maybe (Response docType)
getFromCache request cache =
    let
        mRequestId =
            Dict.toList cache.requests
                |> List.filter (\( id, cachedRequest ) -> cachedRequest == request)
                |> List.map fst
                |> List.head

        mResponse =
            mRequestId
                `Maybe.andThen` (\id ->
                                    Dict.get id cache.responses
                                )
    in
        mResponse


setInCache :
    Request
    -> Response docType
    -> Cache' api docType
    -> Cache' api docType
setInCache request response cache =
    let
        id =
            cache.nextRequestId
    in
        { cache
            | nextRequestId = id + 1
            , requests = Dict.insert id request cache.requests
            , responses = Dict.insert id response cache.responses
        }
