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
        , viewHeader model
        , viewResponse model
        , viewFooter model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    header []
        [ nav []
            [ h1 []
                [ a [ href "#" ] [ text "Les bonnes choses" ] ]
            , ul []
                [ li []
                    [ a [ href "#", onClick (NavigateTo (Bookmark "about")) ] [ text "About" ]
                    , a [ href "#", onClick (NavigateTo (Bookmark "stores")) ] [ text "Stores" ]
                    ]
                ]
            , ul []
                [ li []
                    [ a [ href "#", onClick (NavigateTo (Bookmark "jobs")) ] [ text "Jobs" ]
                    , a [ href "#", onClick (NavigateTo Blog) ] [ text "Blog" ]
                    ]
                ]
            , a [ href "#", onClick (NavigateTo (Form "everything")) ] [ span [] [ text "Search" ] ]
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
