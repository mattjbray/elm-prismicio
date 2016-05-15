module App.View exposing (..)

import App.Types exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (selected)
import Html.Events exposing (onInput)
import Prismic.Types exposing (Response, SearchResult, DefaultDocType)
import Prismic.View exposing (structuredTextAsHtml, asHtml)


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
            option [ selected (formName == model.selectedForm) ] [ text formName ]
    in
        div []
            (case model.api of
                Just (Ok api) ->
                    [ select [ onInput SetSelectedForm ]
                        (List.map viewOption (Dict.keys api.forms))
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
            div []
                [ h1 [] [ text "Response" ]
                , viewResponseOk response
                ]

        Just (Err error) ->
            div []
                [ h1 [] [ text "Error" ]
                , p [] [ text (toString error) ]
                ]


viewResponseOk : Response MyDocument -> Html msg
viewResponseOk response =
    div []
        (List.intersperse (hr [] [])
            (List.map viewDocument
                response.results
            )
        )


viewDocument : SearchResult MyDocument -> Html msg
viewDocument result =
    case result.data of
        Default doc ->
            viewDocumentGeneric doc

        JobOfferDoc doc ->
            viewDocumentJobOffer doc

        BlogPostDoc doc ->
            viewDocumentBlogPost doc


viewDocumentGeneric : DefaultDocType -> Html msg
viewDocumentGeneric doc =
    let
        allDocFields =
            let
                fieldsPerType =
                    Dict.values doc

                fieldsPerField =
                    List.concatMap Dict.values fieldsPerType
            in
                List.concat fieldsPerField
    in
        div []
            (h2 [] (List.map text (Dict.keys doc)) :: (List.map asHtml allDocFields))


viewDocumentJobOffer : JobOffer -> Html msg
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


viewDocumentBlogPost : BlogPost -> Html msg
viewDocumentBlogPost blogPost =
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
