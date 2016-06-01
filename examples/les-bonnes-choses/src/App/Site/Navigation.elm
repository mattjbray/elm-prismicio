module App.Site.Navigation exposing (..)

import App.Site.Types exposing (..)
import App.Site.Products.Navigation as Products
import App.Site.Selections.Navigation as Selections
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


toUrl : Page -> String
toUrl page =
    case page of
        SearchP formName ->
            "search/" ++ formName

        AboutP ->
            "about"

        JobsP ->
            "jobs"

        StoresP ->
            "stores"

        ProductsP productsPage ->
            "products/" ++ Products.toUrl productsPage

        SelectionsP selectionsPage ->
            "selections/" ++ Selections.toUrl selectionsPage

        HomeP ->
            ""


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format SearchP (s "search" </> string)
        , format AboutP (s "about")
        , format JobsP (s "jobs")
        , format StoresP (s "stores")
        , format ProductsP (s "products" </> Products.pageParser)
        , format SelectionsP (s "selections" </> Selections.pageParser)
        , format HomeP (s "")
        ]
