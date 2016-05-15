module Prismic exposing (init, form, withRef, submit)

import Dict
import Http
import Task exposing (Task)
import Prismic.Types exposing (..)
import Prismic.Decoders exposing (..)


init : Url -> Task PrismicError Api
init (Url url) =
    Task.mapError FetchApiError
        (Http.get decodeApi url)


form : String -> Task PrismicError Api -> Task PrismicError (Query ())
form formId apiTask =
    let
        addForm api =
            let
                mForm =
                    Dict.get formId api.forms
            in
                case mForm of
                    Nothing ->
                        Task.fail (FormDoesNotExist formId)

                    Just form ->
                        Task.succeed
                            { api = api
                            , action = form.action
                            , ref = ()
                            }
    in
        apiTask `Task.andThen` addForm


withRef : String -> Task PrismicError (Query ()) -> Task PrismicError (Query Ref)
withRef refId queryTask =
    let
        addRef query =
            let
                mRef =
                    query.api.refs
                        |> List.filter (\ref -> ref.id == refId)
                        |> List.head
            in
                case mRef of
                    Nothing ->
                        Task.fail (RefDoesNotExist refId)

                    Just ref ->
                        Task.succeed { query | ref = ref.ref }
    in
        queryTask `Task.andThen` addRef


submit : Task PrismicError (Query Ref) -> Task PrismicError Response
submit queryTask =
    let
        doSubmit query =
            let
                (Url url) =
                    queryToUrl query
            in
                Task.mapError SubmitQueryError
                    (Http.get decodeResponse url)
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
                [ ( "ref", refStr ) ]
            )
