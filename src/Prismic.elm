module Prismic exposing (fetchApi, form, withRef, submit)

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
    -> Task PrismicError ( Query (), CacheWithApi docType )
form formId apiTask =
    let
        addForm cache =
            let
                mForm =
                    Dict.get formId cache.api.forms
            in
                case mForm of
                    Nothing ->
                        Task.fail (FormDoesNotExist formId)

                    Just form ->
                        let
                            query =
                                Maybe.withDefault ""
                                    (Dict.get "q" form.fields
                                        `Maybe.andThen` .default
                                    )
                        in
                            Task.succeed
                                ( { action = form.action
                                  , ref = ()
                                  , query = query
                                  }
                                , cache
                                )
    in
        apiTask `Task.andThen` addForm


withRef :
    String
    -> Task PrismicError ( Query (), CacheWithApi docType )
    -> Task PrismicError ( Query Ref, CacheWithApi docType )
withRef refId queryTask =
    let
        addRef ( query, cache ) =
            let
                mRef =
                    cache.api.refs
                        |> List.filter (\ref -> ref.id == refId)
                        |> List.head
            in
                case mRef of
                    Nothing ->
                        Task.fail (RefDoesNotExist refId)

                    Just ref ->
                        Task.succeed
                            ( { query | ref = ref.ref }
                            , cache
                            )
    in
        queryTask `Task.andThen` addRef


submit :
    Decoder docType
    -> Task PrismicError ( Query Ref, CacheWithApi docType )
    -> Task PrismicError ( Response docType, Cache docType )
submit decodeDocType queryTask =
    let
        doSubmit ( query, cache ) =
            let
                (Url url) =
                    queryToUrl query
            in
                case getFromCache query cache of
                    Just response ->
                        Task.succeed ( response, { cache | api = Just cache.api } )

                    Nothing ->
                        let
                            cacheWithApi =
                                { cache | api = Just cache.api }

                            mkResponse response =
                                ( response, setInCache query response cacheWithApi )
                        in
                            Http.get (decodeResponse decodeDocType) url
                                |> Task.map mkResponse
                                |> Task.mapError SubmitQueryError
    in
        queryTask `Task.andThen` doSubmit


queryToUrl : Query Ref -> Url
queryToUrl query =
    let
        (Ref refStr) =
            query.ref

        (Url urlStr) =
            query.action
    in
        Url
            (Http.url urlStr
                (( "ref", refStr )
                    :: if String.isEmpty query.query then
                        []
                       else
                        [ ( "q", query.query ) ]
                )
            )


getFromCache :
    Query Ref
    -> Cache' api docType
    -> Maybe (Response docType)
getFromCache query cache =
    let
        mRequestId =
            Dict.toList cache.requests
                |> List.filter (\( id, cachedQuery ) -> cachedQuery == query)
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
    Query Ref
    -> Response docType
    -> Cache' api docType
    -> Cache' api docType
setInCache query response cache =
    let
        id =
            cache.nextRequestId
    in
        { cache
            | nextRequestId = id + 1
            , requests = Dict.insert id query cache.requests
            , responses = Dict.insert id response cache.responses
        }
