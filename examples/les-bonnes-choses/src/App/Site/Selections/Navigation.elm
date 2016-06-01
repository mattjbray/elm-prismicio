module App.Site.Selections.Navigation exposing (..)

import App.Site.Selections.Types exposing (..)
import String
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


toUrl : Page -> String
toUrl page =
    case page of
        ShowP docId slug ->
            String.join "/" [ docId, slug ]


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format ShowP (string </> string)
        ]
