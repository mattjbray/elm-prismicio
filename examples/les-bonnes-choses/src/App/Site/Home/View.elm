module App.Site.Home.View exposing (..)

import App.Blog.Types as Blog
import App.Documents.Types as Documents
import App.Navigation exposing (toHash)
import App.Site.Home.Types exposing (..)
import App.Site.Products.Common.View as Common
import App.Site.Products.Types as Products
import App.Site.Selections.Types as Selections
import App.Site.Types as Site
import App.Types as App
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, href, id, rel, selected, style)
import Html.Events exposing (onClick)
import Prismic.Types exposing (Url(Url))
import Prismic.View exposing (getTitle, getText, getTexts, getFirstParagraph)
import Result.Extra as Result


categories : List Documents.Category
categories =
    [ Documents.Macaron, Documents.Cupcake, Documents.Pie ]


view : Model -> Html Msg
view model =
    div [ class "main", id "home" ]
        [ viewCaroussel model
        , viewFeatured model
        , viewBlog model
        ]


viewCaroussel : Model -> Html Msg
viewCaroussel model =
    section [ id "caroussel" ]
        [ nav []
            [ ul []
                (List.map
                    (\cat ->
                        li []
                            [ a
                                [ classList [ ( "selected", cat == model.category ) ]
                                , onClick (SetCategory cat)
                                ]
                                [ text (Common.categoryToString cat) ]
                            ]
                    )
                    categories
                )
            ]
        , div [ class "products" ]
            [ ul [ class "current" ]
                (model.products
                    |> Result.mapBoth (\err -> [ li [] [ pre [] [ text (toString err) ] ] ])
                        (List.map Common.viewProductShort << filterProducts model.category)
                )
            ]
        , p []
            [ a [ href (toHash (App.SiteP (Site.ProductsP (Products.IndexP Nothing)))) ]
                [ text "Browse all our products" ]
            ]
        ]


filterProducts : Documents.Category -> List Documents.Product -> List Documents.Product
filterProducts category products =
    products
        |> List.filter
            (\product ->
                List.member category product.categories
                    && not (List.member "Featured" product.tags)
            )
        |> List.take 5


viewFeatured : Model -> Html Msg
viewFeatured model =
    section [ id "featured" ]
        (model.featured
            |> Result.mapBoth (\err -> [ div [] [ pre [] [ text (toString err) ] ] ])
                (List.filterMap viewFeaturedItem)
        )


viewFeaturedItem : Featured -> Maybe (Html Msg)
viewFeaturedItem featured =
    case featured of
        ProductF product ->
            Just (Common.viewFeaturedProduct product)

        SelectionF selection ->
            Just (viewFeaturedSelection selection)

        _ ->
            Nothing


viewFeaturedSelection : Documents.Selection -> Html Msg
viewFeaturedSelection selection =
    let
        (Url backgroundImgUrl) =
            selection.catcherImage.views
                |> Dict.get "squarred"
                |> Maybe.map .url
                |> Maybe.withDefault (Url "")
    in
        div [ style [ ( "background-image", "url(" ++ backgroundImgUrl ++ ")" ) ] ]
            [ a [ href (urlForSelection selection) ]
                [ h3 [] [ span [] [ text (getTexts selection.name) ] ]
                , p [] [ span [] [ text (getTexts selection.shortLede) ] ]
                ]
            ]


urlForSelection : Documents.Selection -> String
urlForSelection selection =
    let
        slug =
            selection.slugs
                |> List.head
                |> Maybe.withDefault ""
    in
        (toHash (App.SiteP (Site.SelectionsP (Selections.ShowP selection.id slug))))



getFeaturedBlogPosts : List Featured -> List Documents.BlogPost
getFeaturedBlogPosts =
    List.filterMap
        (\featured ->
            case featured of
                BlogPostF blogPost ->
                    Just blogPost

                _ ->
                    Nothing
        )


viewBlog : Model -> Html Msg
viewBlog model =
    model.featured
        |> Result.mapBoth (\err -> pre [] [ text (toString err) ])
            (Maybe.withDefault (text "")
                << Maybe.map viewFeaturedBlogPost
                << List.head
                << getFeaturedBlogPosts
            )


viewFeaturedBlogPost : Documents.BlogPost -> Html Msg
viewFeaturedBlogPost blogPost =
    let
        blogIndexUrl =
            (toHash (App.BlogP (Blog.IndexP Nothing)))

        slug =
            List.head blogPost.slugs
                |> Maybe.withDefault ""

        blogPostUrl =
            toHash (App.BlogP (Blog.PostP blogPost.id slug))
    in
        section [ id "blog" ]
            [ h2 []
                [ text "Fresh news from "
                , a [ href blogIndexUrl ]
                    [ text "our blog" ]
                ]
            , a [ href blogPostUrl ]
                [ h1 []
                    [ text
                        (blogPost.body
                            |> getTitle
                            |> Maybe.map getText
                            |> Maybe.withDefault ""
                        )
                    ]
                , p []
                    [ text
                        (blogPost.body
                            |> getFirstParagraph
                            |> Maybe.map getText
                            |> Maybe.withDefault ""
                        )
                    ]
                ]
            , a
                [ class "more"
                , href blogPostUrl
                ]
                [ text "Read more" ]
            ]
