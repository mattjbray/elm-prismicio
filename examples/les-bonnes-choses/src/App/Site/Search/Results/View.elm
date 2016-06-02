module App.Site.Search.Results.View exposing (..)

import App.Site.Search.Results.Types exposing (..)
import App.Common exposing (viewError)
import App.Navigation exposing (urlForSelection, urlForProduct)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, href, id, src)
import Regex
import Result.Extra as Result
import Prismic.Types as P
import Prismic.View as P
import String


view : Model -> Html msg
view model =
    section []
        [ model.products
            |> Result.mapBoth viewError
                viewProducts
        , model.articles
            |> Result.mapBoth viewError
                viewArticles
        ]


viewProducts : List ProductR -> Html msg
viewProducts productResults =
    div [ id "product-results", class "products" ]
        [ h2 []
            [ text
                (String.join " "
                    [ "We have"
                    , toString (List.length productResults)
                    , "products matching your search"
                    ]
                )
            ]
        , ul []
            (List.map viewProductR productResults)
        ]


viewProductR : ProductR -> Html msg
viewProductR productR =
    let
        viewItem mkUrl item =
            let
                (P.Url iconImage) =
                    item.image.views
                        |> Dict.get "icon"
                        |> Maybe.map .url
                        |> Maybe.withDefault (P.Url "")
            in
                li []
                    [ a [ href (mkUrl item) ]
                        [ img [ src iconImage ] []
                        , span [] [ item.name |> P.getTexts |> text ]
                        ]
                    ]
    in
        case productR of
            ProductR product ->
                viewItem urlForProduct product

            SelectionR selection ->
                viewItem urlForSelection selection


viewArticles : List ArticleR -> Html msg
viewArticles articleResults =
    div [ id "other-results" ]
        ([ h2 []
            [ text
                (String.join " "
                    [ toString (List.length articleResults)
                    , "relevant articles in our website"
                    ]
                )
            ]
         ]
            ++ List.map viewArticleR articleResults
        )


viewArticleR : ArticleR -> Html msg
viewArticleR articleR =
    -- TODO: add URLs
    case articleR of
        ArticleR myArticle ->
            article []
                [ a []
                    [ h3 [] [ myArticle.title |> P.getTexts |> text ]
                    , em [] [ text "some/url" ]
                    , p [] [ myArticle.content |> P.getTexts |> excerpt |> text ]
                    ]
                ]

        BlogPostR blogPost ->
            article []
                [ a []
                    [ h3 []
                        [ text "In our blog - "
                        , text
                            (blogPost.body
                                |> P.getTitle
                                |> Maybe.map P.getText
                                |> Maybe.withDefault "(no title)"
                            )
                        ]
                    , em [] [ text "some/url" ]
                    , p []
                        [ text
                            (blogPost.body
                                |> P.getFirstParagraph
                                |> Maybe.map (excerpt << P.getText)
                                |> Maybe.withDefault "(no content)"
                            )
                        ]
                    ]
                ]

        StoreR store ->
            article []
                [ a []
                    [ h3 []
                        [ text "Les Bonnes Choses Store - "
                        , store.name |> P.getTexts |> text
                        ]
                    , em [] [ text "some/url" ]
                    , p []
                        [ text
                            (String.join " "
                                [ store.address
                                , store.city
                                , store.zipcode
                                , store.country
                                ]
                            )
                        ]
                    ]
                ]


excerpt : String -> String
excerpt text =
    let
        truncated =
            text
                -- Take the first 500 chars
                |>
                    String.left 500
                -- Split into words
                |>
                    Regex.split Regex.All (Regex.regex "\\s")
                -- Drop the last word (may be split in half)
                |>
                    (\strs ->
                        List.take (List.length strs - 1) strs
                    )
                -- Take the first 50 words.
                |>
                    List.take 50
                -- Join with a space.
                |>
                    String.join " "
    in
        if truncated == text then
            truncated
        else
            truncated ++ "..."
