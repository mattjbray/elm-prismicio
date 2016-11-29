module App.Site.Jobs.Navigation exposing (..)

import App.Site.Jobs.Types exposing (..)
import String
import UrlParser exposing (Parser, (</>), map, oneOf, s, string)


toUrl : Page -> String
toUrl page =
    case page of
        IndexP ->
            ""

        ShowP docId slug ->
            String.join "/" [ docId, slug ]


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ map ShowP (string </> string)
        , map IndexP (s "")
        ]
