module App.Site.Products.Navigation exposing (..)

import App.Site.Products.Types exposing (..)
import String
import UrlParser exposing (Parser, (</>), map, oneOf, s, string)


toUrl : Page -> String
toUrl page =
    case page of
        IndexP Nothing ->
            ""

        IndexP (Just flavour) ->
            String.join "/" [ "by-flavour", flavour ]

        ShowP docId slug ->
            String.join "/" [ docId, slug ]


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ map (IndexP << Just) (s "by-flavour" </> string)
        , map ShowP (string </> string)
        , map (IndexP Nothing) (s "")
        ]
