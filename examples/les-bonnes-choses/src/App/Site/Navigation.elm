module App.Site.Navigation exposing (..)

import App.Site.Types exposing (..)
import App.Site.Jobs.Navigation as Jobs
import App.Site.Products.Navigation as Products
import App.Site.Search.Navigation as Search
import App.Site.Selections.Navigation as Selections
import App.Site.Stores.Navigation as Stores
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


toUrl : Page -> String
toUrl page =
    case page of
        AboutP ->
            "about"

        JobsP jobsPage ->
            "jobs/" ++ Jobs.toUrl jobsPage

        StoresP storesPage ->
            "stores/" ++ Stores.toUrl storesPage

        ProductsP productsPage ->
            "products/" ++ Products.toUrl productsPage

        SearchP searchPage ->
            "search/" ++ Search.toUrl searchPage

        SelectionsP selectionsPage ->
            "selections/" ++ Selections.toUrl selectionsPage

        HomeP ->
            ""


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format AboutP (s "about")
        , format JobsP (s "jobs" </> Jobs.pageParser)
        , format StoresP (s "stores" </> Stores.pageParser)
        , format ProductsP (s "products" </> Products.pageParser)
        , format SearchP (s "search" </> Search.pageParser)
        , format SelectionsP (s "selections" </> Selections.pageParser)
        , format HomeP (s "")
        ]
