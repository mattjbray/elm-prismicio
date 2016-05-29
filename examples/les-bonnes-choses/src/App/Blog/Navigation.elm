module App.Blog.Navigation exposing (..)

import App.Blog.Types exposing (..)
import String
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


toUrl : Page -> String
toUrl page =
    case page of
        IndexP Nothing ->
            ""

        IndexP (Just category) ->
            String.join "/" [ "category", category ]

        PostP docId slug ->
            String.join "/" [ "post", docId, slug ]


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format (IndexP Nothing) (s "")
        , format (IndexP << Just) (s "category" </> string)
        , format PostP (s "post" </> string </> string)
        ]
