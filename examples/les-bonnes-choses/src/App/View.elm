module App.View exposing (..)

import App.Types exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, disabled, href, id, rel, selected, style)
import Html.Events exposing (onClick, onInput)
import Prismic.Types exposing (Response, SearchResult, DefaultDocType, Url(Url))
import Prismic.View exposing (structuredTextAsHtml, asHtml, imageAsHtml, viewDefaultDocType)


view : Model -> Html Msg
view model =
    div []
        [ node "link" [ rel "stylesheet", href "http://lesbonneschoses.prismic.me/assets/stylesheets/normalize.min.css" ] []
        , node "link" [ rel "stylesheet", href "http://lesbonneschoses.prismic.me/assets/stylesheets/main.css" ] []
          --, viewControls model
        , viewHeader model
        , viewResponse model
        , viewFooter model
        ]


viewControls : Model -> Html Msg
viewControls model =
    let
        viewOption formName =
            option [ selected (Form formName == model.selected) ] [ text formName ]

        viewBookmark bookmarkId =
            button
                [ onClick (SetSelected (Bookmark bookmarkId))
                , disabled (Bookmark bookmarkId == model.selected)
                ]
                [ text bookmarkId ]
    in
        div []
            (case model.prismic.api of
                Just api ->
                    List.map viewBookmark (Dict.keys api.bookmarks)
                        ++ [ button
                                [ onClick (SetSelected Blog)
                                , disabled (model.selected == Blog)
                                ]
                                [ text "blog" ]
                           ]
                        ++ [ case model.selected of
                                Form _ ->
                                    select [ onInput (SetSelected << Form) ]
                                        (List.map viewOption (Dict.keys api.forms))

                                _ ->
                                    button
                                        [ onClick (SetSelected (Form "everything"))
                                        , disabled (Form "everything" == model.selected)
                                        ]
                                        [ text "everything" ]
                           ]

                _ ->
                    []
            )


viewHeader : Model -> Html Msg
viewHeader model =
    header []
        [ nav []
            [ h1 []
                [ a [ href "#" ] [ text "Les bonnes choses" ] ]
            , ul []
                [ li []
                    [ a [ href "#", onClick (SetSelected (Bookmark "about")) ] [ text "About" ]
                    , a [ href "#", onClick (SetSelected (Bookmark "stores")) ] [ text "Stores" ]
                    ]
                ]
            , ul []
                [ li []
                    [ a [ href "#", onClick (SetSelected (Bookmark "jobs")) ] [ text "Jobs" ]
                    , a [ href "#", onClick (SetSelected Blog) ] [ text "Blog" ]
                    ]
                ]
            , a [ href "#", onClick (SetSelected (Form "everything")) ] [ span [] [ text "Search" ] ]
            ]
        ]


viewResponse : Model -> Html Msg
viewResponse model =
    case model.response of
        Nothing ->
            p [] [ text "Loading..." ]

        Just (Ok response) ->
            viewResponseOk response model.selected

        Just (Err error) ->
            div []
                [ h1 [] [ text "Error" ]
                , p [] [ text (toString error) ]
                ]


viewResponseOk : Response MyDocument -> Selection -> Html Msg
viewResponseOk response selected =
    div []
        (List.intersperse (hr [] [])
            (List.map (viewDocument selected)
                response.results
            )
        )


viewDocument : Selection -> SearchResult MyDocument -> Html Msg
viewDocument selected result =
    case result.data of
        Default doc ->
            viewDefaultDocType doc

        ArticleDoc doc ->
            viewDocumentArticle doc

        JobOfferDoc doc ->
            viewDocumentJobOffer doc

        BlogPostDoc doc ->
            viewDocumentBlogPost selected doc result.id


viewDocumentArticle : Article -> Html Msg
viewDocumentArticle article =
    let
        (Url imgUrl) =
            article.image.main.url
    in
        div [ class "main", id "about" ]
            [ section [ id "page-header" ]
                [ div [ style [ ( "background-image", "url(" ++ imgUrl ++ ")" ) ] ]
                    [ div []
                        (structuredTextAsHtml article.title
                            ++ structuredTextAsHtml article.shortLede
                        )
                    ]
                ]
            , section [ id "page-body" ]
                (structuredTextAsHtml article.content)
            ]


viewDocumentJobOffer : JobOffer -> Html Msg
viewDocumentJobOffer jobOffer =
    div []
        ((structuredTextAsHtml jobOffer.name)
            ++ [ text
                    (jobOffer.contractType
                        |> Maybe.map (\ct -> ct ++ " position")
                        |> Maybe.withDefault ""
                    )
               , br [] []
               , text
                    (jobOffer.service
                        |> Maybe.map (\service -> service ++ " role")
                        |> Maybe.withDefault ""
                    )
               ]
            ++ (structuredTextAsHtml jobOffer.jobDescription)
            ++ (structuredTextAsHtml jobOffer.profile)
        )


viewDocumentBlogPost : Selection -> BlogPost -> String -> Html Msg
viewDocumentBlogPost selected blogPost docId =
    case selected of
        Blog ->
            viewDocumentBlogPostShort blogPost docId

        _ ->
            viewDocumentBlogPostFull blogPost


viewDocumentBlogPostShort : BlogPost -> String -> Html Msg
viewDocumentBlogPostShort blogPost docId =
    div []
        [ a
            [ onClick (SetSelected (Document docId))
            , href "#"
            ]
            (structuredTextAsHtml blogPost.shortLede)
        , em []
            [ text ("Posted on " ++ blogPost.date ++ " by " ++ blogPost.author ++ " in " ++ blogPost.category) ]
        ]


viewDocumentBlogPostFull : BlogPost -> Html Msg
viewDocumentBlogPostFull blogPost =
    div []
        ([ p [] [ text "BlogPost" ] ]
            ++ (structuredTextAsHtml blogPost.body)
            ++ [ em [] [ text ("Posted on " ++ blogPost.date ++ " by " ++ blogPost.author ++ " in " ++ blogPost.category) ]
               , p []
                    [ text
                        ("Comments are "
                            ++ (if blogPost.allowComments then
                                    "enabled"
                                else
                                    "disabled"
                               )
                            ++ "."
                        )
                    ]
               ]
        )


viewFooter : Model -> Html Msg
viewFooter _ =
    footer []
        [ text "This is a demonstration website for "
        , a [ href "https://github.com/mattjbray/elm-prismicio" ]
            [ text "elm-prismicio" ]
        ]
