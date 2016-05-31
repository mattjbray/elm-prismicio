module Prismic exposing (fetchApi, form, ref, query, bookmark, none, submit, any, at, atL)

import Dict
import Json.Decode as Json exposing (Decoder)
import Http
import Task exposing (Task)
import Prismic.Types exposing (..)
import Prismic.Decoders exposing (..)
import String


getJson : Decoder a -> String -> Task Http.Error a
getJson decoder url =
    let
        request =
            { verb = "GET"
            , headers =
                [ ( "Accept", "application/json" ) ]
            , url = url
            , body = Http.empty
            }
    in
        Http.fromJson decoder (Http.send Http.defaultSettings request)


fetchApi : Cache -> Task PrismicError CacheWithApi
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


form :
    String
    -> Task PrismicError CacheWithApi
    -> Task PrismicError ( Request, CacheWithApi )
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


ref :
    String
    -> Task PrismicError ( Request, CacheWithApi )
    -> Task PrismicError ( Request, CacheWithApi )
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
    List Predicate
    -> Task PrismicError ( Request, CacheWithApi )
    -> Task PrismicError ( Request, CacheWithApi )
query predicates requestTask =
    let
        addQuery ( request, cache ) =
            Task.succeed
                ( { request | q = predicatesToStr predicates }
                , cache
                )
    in
        requestTask `Task.andThen` addQuery


bookmark :
    String
    -> Task PrismicError (CacheWithApi)
    -> Task PrismicError ( Request, CacheWithApi )
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


type Predicate
    = At String String
    | AtL String (List String)
    | Any String (List String)


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
            in
                "[:d = " ++ query ++ "]"
    in
        "[" ++ String.concat (List.map predicateToStr predicates) ++ "]"


at : String -> String -> Predicate
at fragment value =
    At fragment value


atL : String -> List String -> Predicate
atL fragment values =
    AtL fragment values


any : String -> List String -> Predicate
any fragment values =
    Any fragment values


none :
    Task PrismicError ( Request, Cache' api )
    -> Task PrismicError ( Request, Cache' api )
none =
    Task.map identity


submit :
    Decoder docType
    -> Task PrismicError ( Request, CacheWithApi )
    -> Task PrismicError ( Response docType, Cache )
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
    -> Cache' api
    -> Maybe Json.Value
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
    -> Json.Value
    -> Cache' api
    -> Cache' api
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
