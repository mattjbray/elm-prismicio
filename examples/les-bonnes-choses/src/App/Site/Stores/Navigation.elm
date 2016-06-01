module App.Site.Stores.Navigation exposing (..)

import App.Site.Stores.Types exposing (..)
import String
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


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
        [ format ShowP (string </> string)
        , format IndexP (s "")
        ]
