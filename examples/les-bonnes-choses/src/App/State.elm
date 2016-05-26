module App.State exposing (..)

import App.Decoders exposing (decodeMyDocument)
import App.Types exposing (..)
import Navigation
import Prismic as P
import Prismic.Types exposing (Url(Url))
import Prismic.State
import String
import Task
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


initModel : Model
initModel =
  { response =
      Nothing
  , prismic =
      Prismic.State.initCache (Url "https://lesbonneschoses.prismic.io/api")
  , page =
      Bookmark "about"
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



toHash : Page -> String
toHash page =
  case page of
    Blog ->
      "#blog"

    Form formName ->
      "#forms/" ++ formName

    Bookmark "about" ->
      "#about"

    Bookmark "jobs" ->
      "#jobs"

    Bookmark "stores" ->
      "#stores"

    Bookmark bookmarkName ->
      "#bookmarks/" ++ bookmarkName

    Document docId ->
      "#documents/" ++ docId


hashParser : Navigation.Location -> Result String Page
hashParser location =
  UrlParser.parse identity pageParser (String.dropLeft 1 location.hash)


pageParser : Parser (Page -> a) a
pageParser =
  oneOf
    [ format Blog (s "blog")
    , format Form (s "forms" </> string)
    , format (Bookmark "about") (s "about")
    , format (Bookmark "jobs") (s "jobs")
    , format (Bookmark "stores") (s "stores")
    , format Bookmark (s "bookmarks" </> string)
    , format Document (s "documents" </> string)
    ]


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
        Bookmark bookmark ->
            model.prismic
                |> P.fetchApi
                |> P.bookmark bookmark
                |> P.submit decodeMyDocument
                |> Task.perform SetError SetResponse

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
