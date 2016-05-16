module App.View exposing (..)

import App.Types exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (disabled, href, selected)
import Html.Events exposing (onClick, onInput)
import Prismic.Types exposing (Response, SearchResult, DefaultDocType)
import Prismic.View exposing (structuredTextAsHtml, asHtml, imageAsHtml, viewDefaultDocType)


view : Model -> Html Msg
view model =
    div []
        [ viewControls model
        , viewResponse model
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
    div []
        [ structuredTextAsHtml article.title
        , imageAsHtml article.image.main
        , structuredTextAsHtml article.content
        ]


viewDocumentJobOffer : JobOffer -> Html Msg
viewDocumentJobOffer jobOffer =
    div []
        [ structuredTextAsHtml jobOffer.name
        , text
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
        , structuredTextAsHtml jobOffer.jobDescription
        , structuredTextAsHtml jobOffer.profile
        ]


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
            [ structuredTextAsHtml blogPost.shortLede ]
        , em []
            [ text ("Posted on " ++ blogPost.date ++ " by " ++ blogPost.author ++ " in " ++ blogPost.category) ]
        ]


viewDocumentBlogPostFull : BlogPost -> Html Msg
viewDocumentBlogPostFull blogPost =
    div []
        [ p [] [ text "BlogPost" ]
        , structuredTextAsHtml blogPost.body
        , em [] [ text ("Posted on " ++ blogPost.date ++ " by " ++ blogPost.author ++ " in " ++ blogPost.category) ]
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
