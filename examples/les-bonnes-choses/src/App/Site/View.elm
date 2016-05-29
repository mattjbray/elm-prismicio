module App.Site.View exposing (..)

import App.Navigation exposing (toHash)
import App.Site.Types exposing (..)
import App.Site.Article.View as Article
import App.Site.Products.View as Products
import App.Types as App
import App.Blog.Types as Blog
import Html exposing (..)
import Html.App exposing (map)
import Html.Attributes exposing (class, classList, disabled, href, id, rel, selected, style)


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , viewContent model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        mkHeaderLink page linkText =
            a
                [ href (toHash (App.SiteP page))
                , classList [ ( "selected", model.page == page ) ]
                ]
                [ text linkText ]
    in
        header []
            [ nav []
                [ h1 []
                    [ mkHeaderLink AboutP "Les bonnes choses" ]
                , ul []
                    [ li [] [ mkHeaderLink AboutP "About" ]
                    , li [] [ mkHeaderLink StoresP "Stores" ]
                    ]
                , ul []
                    [ li [] [ mkHeaderLink JobsP "Jobs" ]
                    , li []
                        [ a [ href (toHash (App.BlogP (Blog.IndexP Nothing))) ]
                            [ text "Blog" ]
                        ]
                    ]
                , a [ href (toHash (App.SiteP (SearchP "everything"))) ]
                    [ span [] [ text "Search" ] ]
                ]
            ]


viewContent : Model -> Html Msg
viewContent model =
    case model.content of
        ArticleC article ->
            map ArticleMsg (Article.view article)

        ProductsC products ->
            map ProductsMsg (Products.view products)

        NoContent ->
            p [] [ text "No Site page loaded." ]
