module App.Site.View exposing (..)

import App.Navigation exposing (toHash)
import App.Site.Types exposing (..)
import App.Site.Article.View as Article
import App.Site.Home.View as Home
import App.Site.Products.View as Products
import App.Site.Selections.View as Selections
import App.Site.Stores.View as Stores
import App.Site.Stores.Types as Stores
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
                    [ mkHeaderLink HomeP "Les bonnes choses" ]
                , ul []
                    [ li [] [ mkHeaderLink AboutP "About" ]
                    , li [] [ mkHeaderLink (StoresP Stores.IndexP) "Stores" ]
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

        HomeC home ->
            map HomeMsg (Home.view home)

        ProductsC products ->
            map ProductsMsg (Products.view products)

        SelectionsC selections ->
            map SelectionsMsg (Selections.view selections)

        StoresC stores ->
            map StoresMsg (Stores.view stores)

        NoContent ->
            p [] [ text "No Site page loaded." ]
