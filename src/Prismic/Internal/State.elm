module Prismic.Internal.State exposing (..)

import Dict
import Json.Decode as Json exposing (Decoder)
import Http
import Prismic.Types exposing (..)
import String
import Task exposing (Task)


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
    -> Model' api
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
    -> Model' api
    -> Model' api
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
