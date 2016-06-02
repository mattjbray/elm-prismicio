module App.Site.Search.Navigation exposing (..)

import App.Site.Search.Types exposing (..)
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


toUrl : Page -> String
toUrl page =
    case page of
        IndexP ->
            ""

        ResultsP query ->
            query


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format ResultsP string
        , format IndexP (s "")
        ]
