module App.Common exposing (..)

import App.Blog.Types as Blog
import App.Documents.Types as Documents
import App.Navigation exposing (toHash)
import App.Site.Products.Types as Products
import App.Site.Search.Types as Search
import App.Site.Selections.Types as Selections
import App.Site.Stores.Types as Stores
import App.Site.Types as Site
import App.Types as App
import Html exposing (..)
import Html.Attributes exposing (classList, id, href)
import Prismic.Types as P
import Prismic.View as P


linkResolver : P.LinkedDocument -> P.Url
linkResolver linkedDoc =
    let
        page =
            case linkedDoc.linkedDocumentType of
                "blog-post" ->
                    App.BlogP <| Blog.PostP linkedDoc.id linkedDoc.slug

                "store" ->
                    App.SiteP <| Site.StoresP <| Stores.ShowP linkedDoc.id linkedDoc.slug

                _ ->
                    let
                        _ =
                            Debug.log "Cannot resolve linkedDoc: " linkedDoc
                    in
                        App.SiteP <| Site.HomeP
    in
        P.Url (toHash page)


structuredTextAsHtml : P.StructuredText -> List (Html msg)
structuredTextAsHtml =
    P.structuredTextAsHtml linkResolver


toCssUrl : P.Url -> String
toCssUrl (P.Url url) =
    "url(" ++ url ++ ")"


viewLoading : Html msg
viewLoading =
    section [ id "page-header" ]
        [ div []
            [ div []
                [ h1 [] [ text "Loading..." ]
                ]
            ]
        ]


viewError : P.PrismicError -> Html msg
viewError error =
    pre [] [ text (toString error) ]


viewHeader : App.Page -> Html msg
viewHeader currentPage =
    let
        mkHeaderLink page linkText =
            a
                [ href (toHash (App.SiteP page))
                , classList [ ( "selected", currentPage == App.SiteP page ) ]
                ]
                [ text linkText ]
    in
        header []
            [ nav []
                [ h1 []
                    [ mkHeaderLink Site.HomeP "Les bonnes choses" ]
                , ul []
                    [ li [] [ mkHeaderLink Site.AboutP "About" ]
                    , li [] [ mkHeaderLink (Site.StoresP Stores.IndexP) "Stores" ]
                    ]
                , ul []
                    [ li [] [ mkHeaderLink Site.JobsP "Jobs" ]
                    , li []
                        [ a [ href (toHash (App.BlogP (Blog.IndexP Nothing))) ]
                            [ text "Blog" ]
                        ]
                    ]
                , a [ href (toHash (App.SiteP (Site.SearchP Search.IndexP))) ]
                    [ span [] [ text "Search" ] ]
                ]
            ]


urlForProduct : Documents.Product -> String
urlForProduct product =
    let
        slug =
            product.slugs
                |> List.head
                |> Maybe.withDefault ""
    in
        (toHash (App.SiteP (Site.ProductsP (Products.ShowP product.id slug))))


urlForSelection : Documents.Selection -> String
urlForSelection selection =
    let
        slug =
            selection.slugs
                |> List.head
                |> Maybe.withDefault ""
    in
        (toHash (App.SiteP (Site.SelectionsP (Selections.ShowP selection.id slug))))
