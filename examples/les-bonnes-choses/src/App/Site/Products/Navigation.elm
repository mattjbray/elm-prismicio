module App.Site.Products.Navigation exposing (..)

import App.Site.Products.Types exposing (..)
import String
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


toUrl : Page -> String
toUrl page =
    case page of
      IndexP ->
        ""
      ProductP docId slug ->
        String.join "/" [docId, slug]

pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format ProductP (string </> string)
        , format IndexP (s "")
        ]
