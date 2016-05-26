module App.Navigation exposing (..)

import App.Types exposing (..)
import Navigation
import String
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)

toHash : Page -> String
toHash page =
  case page of
    Blog ->
      "#blog"

    Form formName ->
      "#forms/" ++ formName

    About ->
      "#about"

    Jobs ->
      "#jobs"

    Stores ->
      "#stores"

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
    , format About (s "about")
    , format Jobs (s "jobs")
    , format Stores (s "stores")
    , format Document (s "documents" </> string)
    ]
