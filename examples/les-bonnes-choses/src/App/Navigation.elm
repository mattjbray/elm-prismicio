module App.Navigation exposing (..)

import App.Blog.Navigation as Blog
import App.Blog.Types as Blog
import App.Documents.Types as Documents
import App.Site.Navigation as Site
import App.Site.Products.Types as Products
import App.Site.Search.Types as Search
import App.Site.Selections.Types as Selections
import App.Site.Stores.Types as Stores
import App.Site.Types as Site
import App.Types as App
import App.Types exposing (..)
import Dict exposing (Dict)
import Navigation
import Prismic.Types as P
import String
import UrlParser exposing (Parser, (</>), format, oneOf, s, string)


-- Navigation


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



-- Prismic linkResolver


linkResolver : P.LinkedDocument -> P.Url
linkResolver linkedDoc =
    let
        url =
            case linkedDoc.linkedDocumentType of
                "blog-post" ->
                    toHash <| App.BlogP <| Blog.PostP linkedDoc.id linkedDoc.slug

                "store" ->
                    toHash <| App.SiteP <| Site.StoresP <| Stores.ShowP linkedDoc.id linkedDoc.slug

                _ ->
                    let
                        _ =
                            Debug.log "Cannot resolve linkedDoc: " linkedDoc
                    in
                        urlForHome
    in
        P.Url url



-- Url helpers


urlForArticle : Dict String String -> Documents.Article -> String
urlForArticle bookmarks article =
    let
        bookmarksById =
            bookmarks
                |> Dict.toList
                |> List.map (\( k, v ) -> ( v, k ))
                |> Dict.fromList

        mBookmark =
            Dict.get article.id bookmarksById

        page =
            case mBookmark of
                Just "about" ->
                    App.SiteP <| Site.AboutP

                Just "jobs" ->
                    App.SiteP <| Site.JobsP

                Just "stores" ->
                    App.SiteP <| Site.StoresP <| Stores.IndexP

                _ ->
                    App.NotFoundP
    in
        toHash page


urlForBlog : String
urlForBlog =
    toHash <| App.BlogP <| Blog.IndexP Nothing


urlForBlogCategory : String -> String
urlForBlogCategory category =
    toHash <| App.BlogP <| Blog.IndexP <| Just category


urlForBlogPost : Documents.BlogPost -> String
urlForBlogPost blogPost =
    toHash (App.BlogP (Blog.PostP blogPost.id (Maybe.withDefault "post" (List.head blogPost.slugs))))


urlForHome : String
urlForHome =
    toHash <| App.SiteP <| Site.HomeP


urlForProduct : Documents.Product -> String
urlForProduct product =
    let
        slug =
            product.slugs
                |> List.head
                |> Maybe.withDefault ""
    in
        (toHash (App.SiteP (Site.ProductsP (Products.ShowP product.id slug))))


urlForProducts : String
urlForProducts =
    toHash <| App.SiteP <| Site.ProductsP <| Products.IndexP Nothing


urlForProductsByFlavour : String -> String
urlForProductsByFlavour flavour =
    toHash <| App.SiteP <| Site.ProductsP <| Products.IndexP <| Just flavour


urlForSearch : String
urlForSearch =
    toHash <| App.SiteP <| Site.SearchP <| Search.IndexP


urlForSearchResults : String -> String
urlForSearchResults query =
    toHash <| App.SiteP <| Site.SearchP <| Search.ResultsP query


urlForSelection : Documents.Selection -> String
urlForSelection selection =
    let
        slug =
            selection.slugs
                |> List.head
                |> Maybe.withDefault ""
    in
        (toHash (App.SiteP (Site.SelectionsP (Selections.ShowP selection.id slug))))


urlForStore : Documents.Store -> String
urlForStore store =
    let
        slug =
            store.slugs
                |> List.head
                |> Maybe.withDefault ""
    in
        toHash <| App.SiteP <| Site.StoresP <| Stores.ShowP store.id slug
