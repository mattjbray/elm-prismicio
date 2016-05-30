module App.Site.Products.Navigation exposing (..)

import App.Site.Products.Types exposing (..)
import String
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


toUrl : Page -> String
toUrl page =
    case page of
        IndexP Nothing ->
            ""

        IndexP (Just flavour) ->
            String.join "/" [ "by-flavour", flavour ]

        ProductP docId slug ->
            String.join "/" [ docId, slug ]


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format (IndexP << Just) (s "by-flavour" </> string)
        , format ProductP (string </> string)
        , format (IndexP Nothing) (s "")
        ]
