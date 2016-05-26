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
      AboutP
  }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NavigateTo page ->
          model ! [ Navigation.newUrl (toHash page) ]

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
      let newModel = { model | page = page, response = Nothing }
      in newModel ! [ fetchPageFor newModel]


fetchPageFor : Model -> Cmd Msg
fetchPageFor model =
    case model.page of
        AboutP ->
          fetchBookmark model "about"
        JobsP ->
          fetchBookmark model "jobs"
        StoresP ->
          fetchBookmark model "stores"

        SearchP form ->
            model.prismic
                |> P.fetchApi
                |> P.form form
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse

        BlogPostP docId ->
            model.prismic
                |> P.fetchApi
                |> P.form "everything"
                |> P.query ("[[:d = at(document.id, \"" ++ docId ++ "\")]]")
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse

        BlogP ->
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
