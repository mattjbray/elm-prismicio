module App.Site.Home.View exposing (..)

import App.Documents.Types as Documents
import App.Navigation exposing (urlForBlog, urlForBlogPost, urlForProducts, urlForSelection)
import App.Site.Home.Types exposing (..)
import App.Site.Products.Common.View as Common
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, href, id, rel, selected, style)
import Html.Events exposing (onClick)
import Prismic as P exposing (Url(Url))
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
                    |> Result.unpack (\err -> [ li [] [ pre [] [ text (toString err) ] ] ])
                        (List.map Common.viewProductShort << filterProducts model.category)
                )
            ]
        , p []
            [ a [ href urlForProducts ]
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
            |> Result.unpack (\err -> [ div [] [ pre [] [ text (toString err) ] ] ])
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
                [ h3 [] [ span [] [ text (P.getTexts selection.name) ] ]
                , p [] [ span [] [ text (P.getTexts selection.shortLede) ] ]
                ]
            ]


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
        |> Result.unpack (\err -> pre [] [ text (toString err) ])
            (Maybe.withDefault (text "")
                << Maybe.map viewFeaturedBlogPost
                << List.head
                << getFeaturedBlogPosts
            )


viewFeaturedBlogPost : Documents.BlogPost -> Html Msg
viewFeaturedBlogPost blogPost =
    section [ id "blog" ]
        [ h2 []
            [ text "Fresh news from "
            , a [ href urlForBlog ]
                [ text "our blog" ]
            ]
        , a [ href (urlForBlogPost blogPost) ]
            [ h1 []
                [ text
                    (blogPost.body
                        |> P.getTitle
                        |> Maybe.map P.getText
                        |> Maybe.withDefault ""
                    )
                ]
            , p []
                [ text
                    (blogPost.body
                        |> P.getFirstParagraph
                        |> Maybe.map P.getText
                        |> Maybe.withDefault ""
                    )
                ]
            ]
        , a
            [ class "more"
            , href (urlForBlogPost blogPost)
            ]
            [ text "Read more" ]
        ]
