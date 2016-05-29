module App.Site.Products.Navigation exposing (..)

import App.Site.Products.Types exposing (..)
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


toUrl : Page -> String
toUrl page =
    case page of
      IndexP ->
        ""

pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format IndexP (s "")
        ]
