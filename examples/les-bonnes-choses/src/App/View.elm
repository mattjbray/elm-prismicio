module App.View exposing (..)

import App.Navigation exposing (toHash)
import App.Types exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, disabled, href, id, rel, selected, style)
import Html.Events exposing (onInput, onWithOptions, defaultOptions)
import Json.Decode as Json
import Prismic.Types exposing (Response, SearchResult, DefaultDocType, Url(Url))
import Prismic.View exposing (structuredTextAsHtml, asHtml, imageAsHtml, viewDefaultDocType)


onClick msg = onWithOptions "click" {defaultOptions | preventDefault = True} (Json.succeed msg)


view : Model -> Html Msg
view model =
    div []
        [ node "link" [ rel "stylesheet", href "http://lesbonneschoses.prismic.me/assets/stylesheets/normalize.min.css" ] []
        , node "link" [ rel "stylesheet", href "http://lesbonneschoses.prismic.me/assets/stylesheets/main.css" ] []
        , viewHeader model
        , viewResponse model
        , viewFooter model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    header []
        [ nav []
            [ h1 []
                [ a [ href (toHash About), onClick (NavigateTo About) ] [ text "Les bonnes choses" ] ]
            , ul []
                [ li []
                    [ a [ href (toHash About), onClick (NavigateTo About) ] [ text "About" ]
                    , a [ href (toHash Stores), onClick (NavigateTo Stores) ] [ text "Stores" ]
                    ]
                ]
            , ul []
                [ li []
                    [ a [ href (toHash Jobs), onClick (NavigateTo Jobs) ] [ text "Jobs" ]
                    , a [ href (toHash Blog), onClick (NavigateTo Blog) ] [ text "Blog" ]
                    ]
                ]
            , a [ href (toHash (Form "everything")), onClick (NavigateTo (Form "everything")) ] [ span [] [ text "Search" ] ]
            ]
        ]


viewResponse : Model -> Html Msg
viewResponse model =
    case model.response of
        Nothing ->
            p [] [ text "Loading..." ]

        Just (Ok response) ->
            viewResponseOk response model.page

        Just (Err error) ->
            div []
                [ h1 [] [ text "Error" ]
                , p [] [ text (toString error) ]
                ]


viewResponseOk : Response MyDocument -> Page -> Html Msg
viewResponseOk response page =
    div []
        (List.intersperse (hr [] [])
            (List.map (viewDocument page)
                response.results
            )
        )


viewDocument : Page -> SearchResult MyDocument -> Html Msg
viewDocument page result =
    case result.data of
        Default doc ->
            viewDefaultDocType doc

        ArticleDoc doc ->
            viewDocumentArticle doc

        JobOfferDoc doc ->
            viewDocumentJobOffer doc

        BlogPostDoc doc ->
            viewDocumentBlogPost page doc result.id


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
    div [ class "main", id "job" ]
        [ section [ id "page-header" ]
            [ div []
                [ div []
                    (structuredTextAsHtml jobOffer.name)
                ]
            ]
        , section [ id "page-body" ]
            ([ h2 [] [ text "About you" ] ]
                ++ (structuredTextAsHtml jobOffer.profile)
                ++ [ h2 [] [ text "Your responsibilities" ] ]
                ++ (structuredTextAsHtml jobOffer.jobDescription)
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
            )
        ]


viewDocumentBlogPost : Page -> BlogPost -> String -> Html Msg
viewDocumentBlogPost page blogPost docId =
    case page of
        Blog ->
            viewDocumentBlogPostShort blogPost docId

        _ ->
            viewDocumentBlogPostFull blogPost


viewDocumentBlogPostShort : BlogPost -> String -> Html Msg
viewDocumentBlogPostShort blogPost docId =
    div []
        [ a
            [ onClick (NavigateTo (Document docId))
            , href (toHash (Document docId))
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
