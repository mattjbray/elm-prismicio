module App.Site.Search.Navigation exposing (..)

import App.Site.Search.Types exposing (..)
import UrlParser exposing (Parser, (</>), map, oneOf, s, string)


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
        [ map IndexP (s "")
        , map ResultsP string
        ]
