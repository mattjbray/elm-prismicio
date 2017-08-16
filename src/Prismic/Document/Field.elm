module Prismic.Document.Field
    exposing
        ( Decoder
        , DocumentReference
        , Embed
        , Field
        , ImageDimensions
        , ImageView
        , ImageViews
        , Link
        , LinkResolver
        , StructuredText
        , StructuredTextBlock
        , date
        , defaultLinkResolver
        , embedAsHtml
        , getFirstImage
        , getFirstParagraph
        , getText
        , getTexts
        , getTitle
        , image
        , imageAsHtml
        , link
        , linkAsHtml
        , resolveLink
        , structuredText
        , structuredTextAsHtml
        , structuredTextBlockAsHtml
        , text
        )

{-|


## Field types

You can create your own Elm types to represent your documents using the
following components.

@docs Field


### Structured Text

@docs StructuredText, StructuredTextBlock


### Images

@docs ImageViews, ImageView, ImageDimensions


### Embeds

@docs Embed


### Links

@docs Link, DocumentReference


## Decoding fields

@docs Decoder
@docs text, structuredText, image, date, link


## Viewing fields

@docs structuredTextAsHtml, structuredTextBlockAsHtml
@docs imageAsHtml, embedAsHtml, linkAsHtml
@docs LinkResolver, defaultLinkResolver, resolveLink


## `StructuredText` helpers

@docs getTitle, getFirstImage, getFirstParagraph, getText, getTexts

-}

import Date
import Html exposing (Html)
import Html.Attributes exposing (class, href, src)
import Json.Encode
import Prismic.Document.Internal as Internal exposing (..)


-- TYPES


{-| A field in the `Document`.
-}
type alias Field =
    Internal.Field


{-| `StructuredText` can be rendered to HTML using `structuredTextAsHtml`.
-}
type alias StructuredText =
    Internal.StructuredText


{-| An element of `StructuredText`.
-}
type alias StructuredTextBlock =
    Internal.StructuredTextBlock


{-| A collection of image views.
-}
type alias ImageViews =
    Internal.ImageViews


{-| Properties for a single image view.
-}
type alias ImageView =
    Internal.ImageView


{-| Dimensions of an image view.
-}
type alias ImageDimensions =
    Internal.ImageDimensions


{-| Embed elements.
-}
type alias Embed =
    Internal.Embed


{-| Links to other documents or to the web.
-}
type alias Link =
    Internal.Link


{-| A reference to a Prismic document.
-}
type alias DocumentReference =
    Internal.DocumentReference



-- VIEW HELPERS


{-| A `LinkResolver` simply converts a Prismic `DocumentReference` to a list of
`Html.Attribute`s. `structuredTextAsHtml` and friends add these attributes to
links in the text.

For example, you can use this to add `onClick` handlers to links:

    type Msg
        = NavigateTo DocumentReference

    myLinkResolver : LinkResolver Msg
    myLinkResolver docRef =
        [ Html.Attributes.href ""
        , Html.Events.onClick (NavigateTo docRef)
        ]

    view : StructuredText -> Html Msg
    view myStructuredText =
        structuredTextAsHtml myLinkResolver myStructuredText

Your `update` function would handle the `NavigateTo` message and perform the
appropriate routing.

-}
type alias LinkResolver msg =
    { resolveDocumentReference :
        DocumentReference -> List (Html.Attribute msg)
    , resolveUrl :
        String -> List (Html.Attribute msg)
    }


{-| Adds a default `href` attribute to links:
[ href "documents/{doc.id}/{doc.slug}" ]
-}
defaultLinkResolver : LinkResolver msg
defaultLinkResolver =
    { resolveDocumentReference =
        \linkedDoc ->
            [ href (String.join "/" [ "documents", linkedDoc.id, linkedDoc.slug ]) ]
    , resolveUrl =
        \url ->
            [ href url ]
    }


{-| Render some `StructuredText` as HTML.

You must supply a `LinkResolver` to resolve any links in the `StructuredText`.
If you don't care about this, you can use the `defaultLinkResolver`.

-}
structuredTextAsHtml : LinkResolver msg -> StructuredText -> List (Html msg)
structuredTextAsHtml linkResolver (StructuredText blocks) =
    List.map (structuredTextBlockAsHtml linkResolver) blocks


{-| Render a single block of `StructuredText` as HTML.
-}
structuredTextBlockAsHtml : LinkResolver msg -> StructuredTextBlock -> Html msg
structuredTextBlockAsHtml linkResolver field =
    case field of
        SImage image ->
            imageAsHtml image

        SEmbed embed ->
            embedAsHtml embed

        Heading1 block ->
            blockAsHtml Html.h1 linkResolver block

        Heading2 block ->
            blockAsHtml Html.h2 linkResolver block

        Heading3 block ->
            blockAsHtml Html.h3 linkResolver block

        Paragraph block ->
            blockAsHtml Html.p linkResolver block

        ListItem block ->
            blockAsHtml
                (\attrs childs ->
                    Html.ul [] [ Html.li attrs childs ]
                )
                linkResolver
                block


blockAsHtml :
    (List (Html.Attribute msg)
     -> List (Html msg)
     -> Html msg
    )
    -> LinkResolver msg
    -> Block
    -> Html msg
blockAsHtml el linkResolver field =
    let
        spanEl span =
            case span.spanElement of
                Em ->
                    Html.em []

                Strong ->
                    Html.strong []

                Hyperlink link ->
                    Html.a (resolveLink linkResolver link)

        foldFn span ( childs, index ) =
            let
                beginning =
                    String.slice index span.start field.text

                middle =
                    String.slice span.start span.end field.text
            in
            ( childs ++ [ Html.text beginning, spanEl span [ Html.text middle ] ]
            , span.end
            )
    in
    el
        (field.label
            |> Maybe.map (\label -> [ class label ])
            |> Maybe.withDefault []
        )
        (field.spans
            |> List.sortBy .start
            |> List.foldl foldFn ( [], 0 )
            |> (\( childs, index ) -> childs ++ [ Html.text (String.dropLeft index field.text) ])
        )


{-| -}
imageAsHtml : ImageView -> Html msg
imageAsHtml image =
    Html.img [ src image.url ] []


{-| -}
embedAsHtml : Embed -> Html msg
embedAsHtml embed =
    case embed of
        EVideo props ->
            Html.div [ Html.Attributes.property "innerHTML" (Json.Encode.string props.html) ] []

        ERich props ->
            Html.div [ Html.Attributes.property "innerHTML" (Json.Encode.string props.html) ] []


{-| -}
resolveLink : LinkResolver msg -> Link -> List (Html.Attribute msg)
resolveLink linkResolver link =
    case link of
        DocumentLink linkedDoc isBroken ->
            linkResolver.resolveDocumentReference linkedDoc

        WebLink url ->
            linkResolver.resolveUrl url


{-| -}
linkAsHtml : LinkResolver msg -> Link -> Html msg
linkAsHtml linkResolver link =
    let
        attrs =
            resolveLink linkResolver link
    in
    case link of
        DocumentLink linkedDoc isBroken ->
            Html.a attrs [ Html.text (toString linkedDoc.slug) ]

        WebLink url ->
            Html.a attrs [ Html.text url ]


{-| Get the first title out of some `StructuredText`, if there is one.
-}
getTitle : StructuredText -> Maybe StructuredTextBlock
getTitle (StructuredText structuredText) =
    let
        isTitle field =
            case field of
                Heading1 _ ->
                    True

                Heading2 _ ->
                    True

                Heading3 _ ->
                    True

                _ ->
                    False
    in
    List.head (List.filter isTitle structuredText)


{-| Get the first paragraph out of some `StructuredText`, if there is one.
-}
getFirstParagraph : StructuredText -> Maybe StructuredTextBlock
getFirstParagraph (StructuredText structuredText) =
    let
        isParagraph field =
            case field of
                Paragraph _ ->
                    True

                _ ->
                    False
    in
    List.head (List.filter isParagraph structuredText)


{-| Get the first image out of some `StructuredText`, if there is one.
-}
getFirstImage : StructuredText -> Maybe ImageView
getFirstImage (StructuredText structuredText) =
    let
        getImage field =
            case field of
                SImage image ->
                    Just image

                _ ->
                    Nothing
    in
    List.head (List.filterMap getImage structuredText)


{-| Get the contents of a single `StructuredText` element as a `String`.
-}
getText : StructuredTextBlock -> String
getText field =
    case field of
        Heading1 block ->
            block.text

        Heading2 block ->
            block.text

        Heading3 block ->
            block.text

        Paragraph block ->
            block.text

        ListItem block ->
            block.text

        SImage imageField ->
            Maybe.withDefault "<image>" imageField.alt

        SEmbed _ ->
            ""


{-| Get the contents of a some `StructuredText` as a `String`.
-}
getTexts : StructuredText -> String
getTexts (StructuredText fields) =
    fields
        |> List.map getText
        |> String.join " "



-- DECODERS


{-| -}
type alias Decoder a =
    Internal.Decoder Field a


{-| Decode a Text field.
-}
text : Decoder String
text =
    Decoder
        (\field ->
            case field of
                Text text ->
                    Ok text

                _ ->
                    Err ("Expected a Text field, but got '" ++ toString field ++ "'.")
        )


{-| Decode a StructuredText field.
-}
structuredText : Decoder StructuredText
structuredText =
    Decoder
        (\field ->
            case field of
                StructuredTextField x ->
                    Ok x

                _ ->
                    Err ("Expected a StructuredText field, but got '" ++ toString field ++ "'.")
        )


{-| Decode an Image field.
-}
image : Decoder ImageViews
image =
    Decoder
        (\field ->
            case field of
                Image x ->
                    Ok x

                _ ->
                    Err ("Expected an Image field, but got '" ++ toString field ++ "'.")
        )


{-| Decode a Date field.
-}
date : Decoder Date.Date
date =
    Decoder
        (\field ->
            case field of
                Date x ->
                    Ok x

                _ ->
                    Err ("Expected a Date field, but got '" ++ toString field ++ "'.")
        )


{-| Decode a Link field.
-}
link : Decoder Link
link =
    Decoder
        (\field ->
            case field of
                Link x ->
                    Ok x

                _ ->
                    Err ("Expected a Link field, but got '" ++ toString field ++ "'.")
        )
