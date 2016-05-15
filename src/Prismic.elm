module Prismic exposing (..)

import Dict
import Http
import Prismic.Types exposing (..)
import Prismic.Decoders exposing (..)
import Task exposing (Task)


urlToString : Url -> String
urlToString (Url str) =
    str


fetchApi : String -> Task Http.Error Api
fetchApi url =
    Http.get decodeApi url


formToUrl : Form -> Ref -> String
formToUrl form ref =
    let
        f name field params =
            case field.default of
                Nothing ->
                    params

                Just defaultValue ->
                    ( name, defaultValue ) :: params
    in
        Http.url (urlToString form.action)
            (( "ref", ref.ref ) :: (Dict.foldl f [] form.fields))


fetchForm : Api -> String -> String -> Task FetchFormError Response
fetchForm api refId formName =
    let
        mForm =
            Dict.get formName api.forms

        mRef =
            List.head (List.filter (\ref -> ref.id == refId) api.refs)
    in
        case ( mRef, mForm ) of
            ( Just ref, Just form ) ->
                Http.get decodeResponse (formToUrl form ref)
                    `Task.onError` (Task.fail << HttpError)

            ( Nothing, _ ) ->
                Task.fail RefDoesNotExist

            ( _, Nothing ) ->
                Task.fail FormDoesNotExist
