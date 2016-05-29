module App.Blog.Navigation exposing (..)

import App.Blog.Types exposing (..)
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


toUrl : Page -> String
toUrl page =
    case page of
        IndexP ->
            ""

        PostP docId ->
            docId


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format IndexP (s "")
        , format PostP string
        ]
