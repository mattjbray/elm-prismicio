module App.Blog.Post.View exposing (..)

import App.Common exposing (structuredTextAsHtml)
import App.Navigation exposing (toHash)
import App.Types as App
import App.Site.Types as Site
import App.Site.Products.Types as Products
import App.Blog.Post.Types exposing (..)
import App.Blog.Common.View exposing (viewPostInfo, blogPostUrl)
import App.Documents.Types as Documents
import Dict
import Html exposing (..)
import Html.Attributes exposing (id, href, src)
import Prismic.View exposing (getText, getTexts, getTitle)
import Prismic.Types exposing (Url(Url))


view : Model -> Html Msg
view model =
    case model.doc of
        Nothing ->
            p [] [ text "Blog Post is loading..." ]

        Just doc ->
            viewDocumentBlogPostFull doc model


viewDocumentBlogPostFull : Documents.BlogPost -> Model -> Html Msg
viewDocumentBlogPostFull blogPost model =
    let
        viewRelatedPost post =
            let
                title =
                    case getTitle post.body of
                        Nothing ->
                            "No title"

                        Just heading ->
                            getText heading
            in
                li []
                    [ a [ href (blogPostUrl post) ]
                        [ text title ]
                    ]

        viewRelatedProduct product =
            let
                (Url imgUrl) =
                    Dict.get "icon" product.image.views
                        |> Maybe.map .url
                        |> Maybe.withDefault (Url "")

                slug =
                    product.slugs
                        |> List.head
                        |> Maybe.withDefault ""
            in
                li []
                    [ a
                        [ href (toHash <| App.SiteP <| Site.ProductsP <| Products.ShowP product.id slug)
                        ]
                        [ img [ src imgUrl ] []
                        , span [] [ text (getTexts product.name) ]
                        ]
                    ]
    in
        div []
            [ section [ id "post" ]
                [ viewPostInfo blogPost
                , article []
                    (structuredTextAsHtml blogPost.body)
                , h2 [] [ text "These should interest you too" ]
                , ul [] (List.map viewRelatedPost model.relatedPosts)
                ]
            , aside []
                [ h2 [] [ text "Some pastries you should love" ]
                , ul [] (List.map viewRelatedProduct model.relatedProducts)
                ]
            , model.error
                |> Maybe.map (\error -> pre [] [ text (toString error) ])
                |> Maybe.withDefault (text "")
            ]
