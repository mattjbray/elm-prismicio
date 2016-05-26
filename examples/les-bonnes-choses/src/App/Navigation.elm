module App.Navigation exposing (..)

import App.Types exposing (..)
import Navigation
import String
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


toHash : Page -> String
toHash page =
    case page of
        BlogP ->
            "#blog"

        BlogPostP docId ->
            "#blog/" ++ docId

        SearchP formName ->
            "#search/" ++ formName

        AboutP ->
            "#about"

        JobsP ->
            "#jobs"

        StoresP ->
            "#stores"


hashParser : Navigation.Location -> Result String Page
hashParser location =
    UrlParser.parse identity pageParser (String.dropLeft 1 location.hash)


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format BlogPostP (s "blog" </> string)
        , format BlogP (s "blog")
        , format SearchP (s "search" </> string)
        , format AboutP (s "about")
        , format JobsP (s "jobs")
        , format StoresP (s "stores")
        ]
