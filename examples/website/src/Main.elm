module Main exposing (main)

import Documents.Homepage
    exposing
        ( BodySlice(..)
        , GalleryGroup
        , HighlightGroup
        , Homepage
        , decodeHomepage
        )
import Documents.Menu exposing (Menu, decodeMenu)
import Html exposing (Html)
import Html.Attributes as Html
import Prismic
import Prismic.Api as Prismic
import Prismic.Document as Prismic
import Prismic.Url exposing (Url(Url))
import String
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias Model =
    { prismic : Prismic.Model
    , doc :
        Maybe Homepage
    , menu : Maybe Menu
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { prismic =
                Prismic.init (Url "https://mattjbray-testing.prismic.io/api")
            , doc = Nothing
            , menu = Nothing
            }
    in
    ( model, fetchHomePage model.prismic )



-- model ! []


type alias PrismicResult a =
    Result Prismic.PrismicError ( Prismic.Response a, Prismic.Model )


type Msg
    = HomepageResponse (PrismicResult Homepage)
    | MenuResponse (PrismicResult Menu)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomepageResponse (Ok ( result, prismic )) ->
            let
                newPrismic =
                    Prismic.cache model.prismic prismic
            in
            ( { model
                | prismic =
                    newPrismic
                , doc =
                    result.results
                        |> List.head
                        |> Maybe.map .data
              }
            , fetchMenu newPrismic
            )

        HomepageResponse (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
            model ! []

        MenuResponse (Ok ( result, prismic )) ->
            ( { model
                | prismic =
                    Prismic.cache model.prismic prismic
                , menu =
                    result.results
                        |> List.head
                        |> Maybe.map .data
              }
            , Cmd.none
            )

        MenuResponse (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
            model ! []


fetchHomePage : Prismic.Model -> Cmd Msg
fetchHomePage prismic =
    Prismic.api prismic
        |> Prismic.form "everything"
        |> Prismic.query [ Prismic.at "my.homepage.uid" "homepage" ]
        |> Prismic.submit decodeHomepage
        |> Task.attempt HomepageResponse


fetchMenu : Prismic.Model -> Cmd Msg
fetchMenu prismic =
    Prismic.api prismic
        |> Prismic.form "everything"
        |> Prismic.query [ Prismic.at "my.menu.uid" "main-nav" ]
        |> Prismic.submit decodeMenu
        |> Task.attempt MenuResponse


view : Model -> Html Msg
view model =
    Html.div []
        [ model.doc
            |> Maybe.map (viewHomepage model.menu)
            |> Maybe.withDefault (Html.text "")
        ]


asHtml : Prismic.StructuredText -> List (Html msg)
asHtml =
    Prismic.structuredTextAsHtml Prismic.defaultLinkResolver


viewHomepage : Maybe Menu -> Homepage -> Html Msg
viewHomepage menu homepage =
    Html.div [ Html.class "homepage" ]
        [ viewHeader menu
        , viewBanner homepage
        , Html.div [ Html.class "container" ]
            (List.map viewBodySlice homepage.body)
        ]


viewHeader : Maybe Menu -> Html msg
viewHeader menu =
    let
        viewLink link =
            Html.li []
                [ Html.a [ Html.href "/" ] [ Html.text link.label ] ]
    in
    Html.header [ Html.class "site-header" ]
        [ Html.a [ Html.href "./" ]
            [ Html.div
                [ Html.class "logo" ]
                [ Html.text
                    (menu
                        |> Maybe.map .title
                        |> Maybe.map Prismic.getTexts
                        |> Maybe.withDefault "..."
                    )
                ]
            ]
        , Html.nav
            []
            [ Html.ul []
                (menu
                    |> Maybe.map .links
                    |> Maybe.withDefault []
                    |> List.map viewLink
                )
            ]
        ]


viewBanner : Homepage -> Html msg
viewBanner homepage =
    let
        (Url imgSrc) =
            homepage.backgroundImage.main.url
    in
    Html.section
        [ Html.class "homepage-banner"
        , Html.attribute "style"
            ([ "background-image: linear-gradient(rgba(0, 0, 0, 0.4)"
             , "rgba(0, 0, 0, 0.6))"
             , "url(" ++ imgSrc ++ ")"
             ]
                |> String.join ", "
            )
        ]
        [ Html.div [ Html.class "banner-content container" ]
            [ Html.h2 [ Html.class "banner-title" ]
                [ Html.text (Prismic.getTexts homepage.title) ]
            , Html.p [ Html.class "banner-description" ]
                [ Html.text (Prismic.getTexts homepage.tagline) ]
            , Html.a [ Html.class "banner-button", Html.href "/about" ]
                [ Html.text "Learn more" ]
            ]
        ]


viewBodySlice : BodySlice -> Html msg
viewBodySlice bodySlice =
    case bodySlice of
        TextSection label text ->
            viewTextSection label text

        Highlight groups ->
            viewHighlights groups

        FullWidthImage image ->
            viewFullWidthImage image

        Gallery groups ->
            viewGallery groups


viewTextSection : Maybe String -> Prismic.StructuredText -> Html msg
viewTextSection label text =
    let
        sectionClass =
            "text-section-"
                ++ Maybe.withDefault "1col" label
    in
    Html.section
        [ Html.class "content-section"
        , Html.class sectionClass
        ]
        (asHtml text)


viewHighlights : List HighlightGroup -> Html msg
viewHighlights groups =
    let
        viewGroup : HighlightGroup -> Html msg
        viewGroup group =
            let
                (Url imgSrc) =
                    group.image.main.url
            in
            Html.section
                [ Html.class "highlight"
                , Html.class "content-section"
                ]
                [ Html.div [ Html.class "highlight-left" ]
                    (List.concat
                        [ asHtml group.title
                        , asHtml group.headline
                        , Maybe.map2
                            (\link linkText ->
                                [ Html.p []
                                    [ Html.a
                                        [ Html.href (toString link) ]
                                        [ Html.text linkText ]
                                    ]
                                ]
                            )
                            group.link
                            group.linkText
                            |> Maybe.withDefault []
                        ]
                    )
                , Html.div [ Html.class "highlight-right" ]
                    [ Html.img [ Html.src imgSrc ] [] ]
                ]
    in
    Html.div []
        (List.map viewGroup groups)


viewFullWidthImage : Prismic.ImageViews -> Html msg
viewFullWidthImage image =
    let
        (Url imgSrc) =
            image.main.url
    in
    Html.section [ Html.class "full-width-image", Html.class "content-section" ]
        [ Html.img [ Html.src imgSrc ] [] ]


viewGallery : List GalleryGroup -> Html msg
viewGallery groups =
    let
        viewItem : GalleryGroup -> Html msg
        viewItem item =
            let
                (Url imgSrc) =
                    item.image.main.url
            in
            Html.div [ Html.class "gallery-item" ]
                (Html.img [ Html.src imgSrc ] []
                    :: asHtml item.description
                )
    in
    Html.section [ Html.class "gallery", Html.class "content-section" ]
        (List.map viewItem groups)
