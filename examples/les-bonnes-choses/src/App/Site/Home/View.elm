module App.Site.Home.View exposing (..)

import App.Blog.Types as Blog
import App.Documents.Types as Documents
import App.Navigation exposing (toHash)
import App.Site.Home.Types exposing (..)
import App.Site.Products.Common.View as Common
import App.Site.Products.Types as Products
import App.Site.Types as Site
import App.Types as App
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, href, id, rel, selected, style)
import Html.Events exposing (onClick)
import Result.Extra as Result
import Prismic.View exposing (getTitle, getText, getFirstParagraph)


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
                (List.map Common.viewFeaturedProduct << getFeaturedProducts)
        )


getFeaturedProducts : List Featured -> List Documents.Product
getFeaturedProducts =
    List.filterMap
        (\featured ->
            case featured of
                Product product ->
                    Just product

                _ ->
                    Nothing
        )


getFeaturedBlogPosts : List Featured -> List Documents.BlogPost
getFeaturedBlogPosts =
    List.filterMap
        (\featured ->
            case featured of
                BlogPost blogPost ->
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
