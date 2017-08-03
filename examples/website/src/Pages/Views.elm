module Pages.Views exposing (linkAttrs, viewBodySlice, viewHeader)

import Documents.Homepage exposing (BodySlice(..), GalleryGroup, HighlightGroup, Homepage)
import Documents.Menu exposing (Menu)
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Prismic.Document as Prismic
import Prismic.Url exposing (Url(Url))


asHtml : Prismic.StructuredText -> List (Html msg)
asHtml =
    Prismic.structuredTextAsHtml Prismic.defaultLinkResolver


linkAttrs : Prismic.Link -> List (Html.Attribute Prismic.DocumentReference)
linkAttrs link =
    case link of
        Prismic.DocumentLink ref _ ->
            [ Html.onClick ref, Html.href "#" ]

        Prismic.WebLink (Url url) ->
            [ Html.href url ]


viewHeader : Menu -> Html Prismic.DocumentReference
viewHeader menu =
    let
        viewLink link =
            Html.li []
                [ Html.a
                    (linkAttrs link.link)
                    [ Html.text link.label ]
                ]
    in
    Html.header [ Html.class "site-header" ]
        [ Html.a [ Html.href "./" ]
            [ Html.div
                [ Html.class "logo" ]
                [ Html.text
                    (Prismic.getTexts menu.title)
                ]
            ]
        , Html.nav
            []
            [ Html.ul []
                (List.map viewLink menu.links)
            ]
        ]


viewBodySlice : BodySlice -> Html msg
viewBodySlice bodySlice =
    case bodySlice of
        Heading text ->
            viewHeading text

        TextSection label text ->
            viewTextSection label text

        Highlight groups ->
            viewHighlights groups

        FullWidthImage image ->
            viewFullWidthImage image

        Gallery groups ->
            viewGallery groups


viewHeading : Prismic.StructuredText -> Html msg
viewHeading text =
    Html.div [] (asHtml text)


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