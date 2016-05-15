module Prismic exposing (Model, Msg, init, update)

import Dict
import Http
import Prismic.State
import Prismic.Types exposing (..)
import Prismic.Decoders exposing (..)
import Task exposing (Task)


type alias Model = Prismic.Types.Model
type alias Msg = Prismic.Types.Msg


init : Url -> ( Model, Cmd Msg )
init = Prismic.State.init


update : Msg -> Model -> ( Model, Cmd Msg )
update = Prismic.State.update


urlToString : Url -> String
urlToString (Url str) =
    str


formToUrl : Form -> Ref -> String
formToUrl form (Ref ref) =
    let
        f name field params =
            case field.default of
                Nothing ->
                    params

                Just defaultValue ->
                    ( name, defaultValue ) :: params
    in
        Http.url (urlToString form.action)
            (( "ref", ref ) :: (Dict.foldl f [] form.fields))


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
                Http.get decodeResponse (formToUrl form ref.ref)
                    `Task.onError` (Task.fail << HttpError)

            ( Nothing, _ ) ->
                Task.fail RefDoesNotExist

            ( _, Nothing ) ->
                Task.fail FormDoesNotExist


getRefByLabel : String -> Api -> Maybe RefProperties
getRefByLabel label api =
    List.head (List.filter (\ref -> ref.label == label) api.refs)



--form : String -> Model -> Result PrismicError Query
