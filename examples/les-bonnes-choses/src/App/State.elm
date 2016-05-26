module App.State exposing (..)

import App.Decoders exposing (decodeMyDocument)
import App.Types exposing (..)
import App.Navigation exposing (toHash)
import Navigation
import Prismic as P
import Prismic.Types exposing (Url(Url))
import Prismic.State
import Task


initModel : Model
initModel =
  { response =
      Nothing
  , prismic =
      Prismic.State.initCache (Url "https://lesbonneschoses.prismic.io/api")
  , page =
      About
  }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NavigateTo page ->
            { model
                | page = page
                , response = Nothing
            } ! [ Navigation.newUrl (toHash page)]

        SetResponse ( response, cache ) ->
            ( { model
                | response = Just (Ok response)
                , prismic = cache
              }
            , Cmd.none
            )

        SetError err ->
            ( { model | response = Just (Err err) }
            , Cmd.none
            )


init : Result String Page -> (Model, Cmd Msg)
init result =
  urlUpdate result initModel


urlUpdate : Result String Page -> Model -> (Model, Cmd Msg)
urlUpdate result model =
  case Debug.log "result" result of
    Err _ ->
      (model, Navigation.modifyUrl (toHash model.page))
    Ok page ->
      { model
          | page = page
          , response = Nothing
      } ! [fetchPage model]


fetchPage : Model -> Cmd Msg
fetchPage model =
    case model.page of
        About ->
          fetchBookmark model "about"
        Jobs ->
          fetchBookmark model "jobs"
        Stores ->
          fetchBookmark model "stores"

        Form form ->
            model.prismic
                |> P.fetchApi
                |> P.form form
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse

        Document docId ->
            model.prismic
                |> P.fetchApi
                |> P.form "everything"
                |> P.query ("[[:d = at(document.id, \"" ++ docId ++ "\")]]")
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse

        Blog ->
            model.prismic
                |> P.fetchApi
                |> P.form "blog"
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse


fetchBookmark : Model -> String -> Cmd Msg
fetchBookmark model bookmarkName  =
  model.prismic
      |> P.fetchApi
      |> P.bookmark bookmarkName
      |> P.submit decodeMyDocument
      |> Task.perform SetError SetResponse
