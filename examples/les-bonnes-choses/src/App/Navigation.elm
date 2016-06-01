module App.Navigation exposing (..)

import App.Types exposing (..)
import App.Blog.Navigation as Blog
import App.Site.Navigation as Site
import Navigation
import String
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


toHash : Page -> String
toHash page =
    case page of
        BlogP blogPage ->
            "#blog/" ++ Blog.toUrl blogPage

        SiteP sitePage ->
            "#" ++ Site.toUrl sitePage

        NotFoundP ->
            "#404"


hashParser : Navigation.Location -> Result String Page
hashParser location =
    UrlParser.parse identity pageParser (String.dropLeft 1 location.hash)


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format BlogP (s "blog" </> Blog.pageParser)
        , format SiteP Site.pageParser
        ]
