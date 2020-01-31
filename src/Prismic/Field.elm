module Prismic.Field exposing
    ( Field
    , StructuredText, StructuredTextBlock
    , ImageViews, ImageView, ImageDimensions
    , Embed
    , Link, DocumentReference
    , GeoPoint
    , text, structuredText, image, date, timestamp, link, geoPoint, select, color, boolean, number
    , structuredTextAsHtml, structuredTextBlockAsHtml
    , imageAsHtml, embedAsHtml, linkAsHtml
    , LinkResolver, defaultLinkResolver, resolveLink
    , getTitle, getFirstImage, getFirstParagraph, getText, getTexts
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


### Misc

@docs GeoPoint


## Decoding fields

@docs text, structuredText, image, date, timestamp, link, geoPoint, select, color, boolean, number


## Viewing fields

@docs structuredTextAsHtml, structuredTextBlockAsHtml
@docs imageAsHtml, embedAsHtml, linkAsHtml
@docs LinkResolver, defaultLinkResolver, resolveLink


## `StructuredText` helpers

@docs getTitle, getFirstImage, getFirstParagraph, getText, getTexts

-}

import Html exposing (Html)
import Html.Attributes exposing (class, href, src)
import Html.Parser
import Html.Parser.Util
import Prismic.Internal as Internal exposing (..)
import Time



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


{-| A reference to a Prismic GeoPoint.
-}
type alias GeoPoint =
    Internal.Point



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
        SImage simage ->
            imageAsHtml simage

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

        OListItem block ->
            blockAsHtml
                (\attrs childs ->
                    Html.ol [] [ Html.li attrs childs ]
                )
                linkResolver
                block

        Preformatted block ->
            blockAsHtml Html.pre linkResolver block


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

                Hyperlink hlink ->
                    Html.a (resolveLink linkResolver hlink)

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
imageAsHtml imageView =
    Html.img [ src imageView.url ] []


{-| -}
embedAsHtml : Embed -> Html msg
embedAsHtml embed =
    parseHtml <|
        case embed of
            EVideo props ->
                props.html

            ERich props ->
                props.html


parseHtml : String -> Html msg
parseHtml code =
    code
        |> Html.Parser.run
        |> Result.map (Html.Parser.Util.toVirtualDom >> Html.div [])
        |> Result.withDefault (Html.div [] [ Html.text code ])


{-| -}
resolveLink : LinkResolver msg -> Link -> List (Html.Attribute msg)
resolveLink linkResolver l =
    case l of
        DocumentLink linkedDoc isBroken ->
            linkResolver.resolveDocumentReference linkedDoc

        WebLink url ->
            linkResolver.resolveUrl url


{-| -}
linkAsHtml : LinkResolver msg -> Link -> Html msg
linkAsHtml linkResolver l =
    let
        attrs =
            resolveLink linkResolver l
    in
    case l of
        DocumentLink linkedDoc isBroken ->
            Html.a attrs [ Html.text linkedDoc.slug ]

        WebLink url ->
            Html.a attrs [ Html.text url ]


{-| Get the first title out of some `StructuredText`, if there is one.
-}
getTitle : StructuredText -> Maybe StructuredTextBlock
getTitle (StructuredText st) =
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
    List.head (List.filter isTitle st)


{-| Get the first paragraph out of some `StructuredText`, if there is one.
-}
getFirstParagraph : StructuredText -> Maybe StructuredTextBlock
getFirstParagraph (StructuredText st) =
    let
        isParagraph field =
            case field of
                Paragraph _ ->
                    True

                _ ->
                    False
    in
    List.head (List.filter isParagraph st)


{-| Get the first image out of some `StructuredText`, if there is one.
-}
getFirstImage : StructuredText -> Maybe ImageView
getFirstImage (StructuredText st) =
    let
        getImage field =
            case field of
                SImage simage ->
                    Just simage

                _ ->
                    Nothing
    in
    List.head (List.filterMap getImage st)


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

        OListItem block ->
            block.text

        Preformatted block ->
            block.text


{-| Get the contents of a some `StructuredText` as a `String`.
-}
getTexts : StructuredText -> String
getTexts (StructuredText fields) =
    fields
        |> List.map getText
        |> String.join " "



-- DECODERS


{-| Decode a Text field.
-}
text : Decoder Field String
text =
    Decoder
        (\field ->
            case field of
                Text ftext ->
                    Ok ftext

                _ ->
                    Err ("Expected a Text field, but got '" ++ Internal.fieldTypeToString field ++ "'.")
        )


{-| Decode a StructuredText field.
-}
structuredText : Decoder Field StructuredText
structuredText =
    Decoder
        (\field ->
            case field of
                StructuredTextField x ->
                    Ok x

                _ ->
                    Err ("Expected a StructuredText field, but got '" ++ Internal.fieldTypeToString field ++ "'.")
        )


{-| Decode an Image field.
-}
image : Decoder Field ImageViews
image =
    Decoder
        (\field ->
            case field of
                Image x ->
                    Ok x

                _ ->
                    Err ("Expected an Image field, but got '" ++ Internal.fieldTypeToString field ++ "'.")
        )


{-| Decode a Date field.
-}
date : Decoder Field Time.Posix
date =
    Decoder
        (\field ->
            case field of
                Date x ->
                    Ok x

                _ ->
                    Err ("Expected a Date field, but got '" ++ Internal.fieldTypeToString field ++ "'.")
        )


{-| Decode a Link field.
-}
link : Decoder Field Link
link =
    Decoder
        (\field ->
            case field of
                Link x ->
                    Ok x

                _ ->
                    Err ("Expected a Link field, but got '" ++ Internal.fieldTypeToString field ++ "'.")
        )


{-| Decode an Select field.
-}
select : Decoder Field String
select =
    Decoder
        (\field ->
            case field of
                Select x ->
                    Ok x

                _ ->
                    Err ("Expected a Link field, but got '" ++ Internal.fieldTypeToString field ++ "'.")
        )


{-| Decode a Timestamp field.
-}
timestamp : Decoder Field Time.Posix
timestamp =
    Decoder
        (\field ->
            case field of
                Timestamp x ->
                    Ok x

                _ ->
                    Err ("Expected a Timestamp field, but got '" ++ Internal.fieldTypeToString field ++ "'.")
        )


{-| Decode a Color field.
-}
color : Decoder Field String
color =
    Decoder
        (\field ->
            case field of
                Color x ->
                    Ok x

                _ ->
                    Err ("Expected a Color field, but got '" ++ Internal.fieldTypeToString field ++ "'.")
        )


{-| Decode a GeoPoint field.
-}
geoPoint : Decoder Field GeoPoint
geoPoint =
    Decoder
        (\field ->
            case field of
                Geo point ->
                    Ok point

                _ ->
                    Err ("Expected a GeoPoint field, but got '" ++ Internal.fieldTypeToString field ++ "'.")
        )


{-| Decode a Boolean field.
-}
boolean : Decoder Field Bool
boolean =
    Decoder
        (\field ->
            case field of
                Boolean x ->
                    Ok x

                _ ->
                    Err ("Expected a Boolean field, but got '" ++ Internal.fieldTypeToString field ++ "'.")
        )


{-| Decode a Number field.
-}
number : Decoder Field Float
number =
    Decoder
        (\field ->
            case field of
                Number x ->
                    Ok x

                _ ->
                    Err ("Expected a Float field, but got '" ++ Internal.fieldTypeToString field ++ "'.")
        )
