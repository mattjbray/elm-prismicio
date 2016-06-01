module App.Site.Navigation exposing (..)

import App.Site.Types exposing (..)
import App.Site.Products.Navigation as Products
import App.Site.Selections.Navigation as Selections
import App.Site.Stores.Navigation as Stores
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

        StoresP storesPage ->
            "stores/" ++ Stores.toUrl storesPage

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
        , format StoresP (s "stores" </> Stores.pageParser)
        , format ProductsP (s "products" </> Products.pageParser)
        , format SelectionsP (s "selections" </> Selections.pageParser)
        , format HomeP (s "")
        ]
