module App.View exposing (..)

import App.Navigation exposing (toHash)
import App.Types exposing (..)
import App.Article.View as Article
import App.Blog.View as Blog
import App.Blog.Types as Blog
import Html exposing (..)
import Html.App exposing (map)
import Html.Attributes exposing (class, classList, disabled, href, id, rel, selected, style)
import Html.Events exposing (onInput, onWithOptions, defaultOptions)
import Json.Decode as Json


onClick : a -> Attribute a
onClick msg =
    onWithOptions "click" { defaultOptions | preventDefault = True } (Json.succeed msg)


view : Model -> Html Msg
view model =
    div []
        [ node "link" [ rel "stylesheet", href "http://lesbonneschoses.prismic.me/assets/stylesheets/normalize.min.css" ] []
        , node "link" [ rel "stylesheet", href "http://lesbonneschoses.prismic.me/assets/stylesheets/main.css" ] []
        , viewHeader model
        , viewContent model
        , viewFooter model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        mkHeaderLink page linkText =
            a
                [ href (toHash page)
                , onClick (NavigateTo page)
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
                    , li [] [ mkHeaderLink (BlogP Blog.IndexP) "Blog" ]
                    ]
                , a
                    [ href (toHash (SearchP "everything"))
                    , onClick (NavigateTo (SearchP "everything"))
                    ]
                    [ span [] [ text "Search" ] ]
                ]
            ]


viewContent : Model -> Html Msg
viewContent model =
    case model.content of
      ArticleC article ->
        map ArticleMsg (Article.view article)

      BlogC blog ->
        map BlogMsg (Blog.view blog)

      NoContent ->
        p [] [ text "No page loaded." ]



viewFooter : Model -> Html Msg
viewFooter _ =
    footer []
        [ text "This is a demonstration website for "
        , a [ href "https://github.com/mattjbray/elm-prismicio" ]
            [ text "elm-prismicio" ]
        ]
