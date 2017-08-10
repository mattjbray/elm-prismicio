module Prismic.Document.Internal exposing (..)

{-|


## Documents

@docs Document


### Field types

You can create your own Elm types to represent your documents using the
following components.


#### Structured Text

@docs StructuredText, StructuredTextBlock


#### Images

@docs ImageViews, ImageView, ImageDimensions


#### Embeds

@docs Embed, EmbedRich, EmbedVideo


#### Links

@docs Link, DocumentReference


## Decoding documents

@docs Decoder, decode, map
@docs FieldDecoder, field, required, optional
@docs text, structuredText, image, date, link
@docs Slice.Decoder, sliceZone, sliceV1, labelledSlice, slice, group


## Viewing documents

@docs structuredTextAsHtml, structuredTextBlockAsHtml
@docs LinkResolver, defaultLinkResolver


### `StructuredText` helpers

@docs getTitle, getFirstImage, getFirstParagraph, getText, getTexts


## Internal

JSON decoders used internally by `elm-prismicio`.

@docs decodeDocument, decodeDocumentJson, decodeDocumentReferenceJson

-}

import Date
import Dict exposing (Dict)
import Json.Decode as Json
import Json.Decode.Pipeline as Json


{-| Holds the Prismic document.

You will decode this into your own document type by passing a `Decoder MyDoc` to
`submit`.

-}
type Document
    = Document (Dict String DocumentField)


{-| A field in the `Document`.
-}
type DocumentField
    = Field Field
    | SliceZone SliceZone


type Field
    = Text String
    | StructuredTextField StructuredText
    | Select String
    | Color String
    | Image ImageViews
    | Number Float
    | Date Date.Date
    | Link Link
    | Groups (List Group)


{-| `StructuredText` can be rendered to HTML using `structuredTextAsHtml`.

TODO: Custom rendering.

-}
type StructuredText
    = StructuredText (List StructuredTextBlock)


{-| An element of `StructuredText`.
-}
type StructuredTextBlock
    = Heading1 Block
    | Heading2 Block
    | Heading3 Block
    | Paragraph Block
    | ListItem Block
    | SImage ImageView
    | SEmbed Embed


{-| Contents of `StructuredText` blocks, such as headings and paragraphs.
-}
type alias Block =
    { text : String
    , spans : List Span
    , label : Maybe String
    }


{-| `Span`s are nested within `StructuredText` blocks.
-}
type alias Span =
    { start : Int
    , end : Int
    , spanElement : SpanElement
    }


{-| Types of spans.
-}
type SpanElement
    = Em
    | Strong
    | Hyperlink Link


{-| A collection of image views.
-}
type alias ImageViews =
    { main : ImageView
    , views : Dict String ImageView
    }


{-| Properties for Html.a single image view.
-}
type alias ImageView =
    { alt : Maybe String
    , copyright : Maybe String
    , url : String
    , dimensions : ImageDimensions
    }


{-| Dimensions of an image view.
-}
type alias ImageDimensions =
    { width : Int
    , height : Int
    }


{-| Embed elements.

TODO: Consolidate Embed types?

-}
type Embed
    = EVideo EmbedVideo
    | ERich EmbedRich


{-| Video embed elements.
-}
type alias EmbedVideo =
    { authorName : String
    , authorUrl : String
    , embedUrl : String
    , height : Int
    , html : String
    , providerName : String
    , providerUrl : String
    , thumbnailHeight : Int
    , thumbnailUrl : String
    , thumbnailWidth : Int
    , title : String
    , version : String
    , width : Int
    }


{-| Rich embed elements.
-}
type alias EmbedRich =
    { authorName : String
    , authorUrl : String
    , cacheAge : String
    , embedUrl : String
    , height : Maybe Int
    , html : String
    , providerName : String
    , providerUrl : String
    , title : String
    , url : String
    , version : String
    , width : Int
    }


{-| Links to other documents or to the web.
-}
type Link
    = DocumentLink DocumentReference Bool
    | WebLink String


{-| A reference to Html.a Prismic document.
-}
type alias DocumentReference =
    { id : String
    , uid : Maybe String
    , slug : String
    , tags : List String
    , linkedDocumentType : String
    }


type alias Group =
    Dict String Field


type alias SliceZone =
    List Slice


{-| A Slice
-}
type alias Slice =
    { sliceLabel : Maybe String
    , sliceType : String
    , sliceContent : SliceContentVersion
    }


type SliceContentVersion
    = -- Deprecated slice format.
      SliceContentField Field
    | -- New slices: repeating and non-repeating parts.
      SliceContent Group (List Group)



-- JSON DECODERS


{-| Decode a `Document` from JSON.
-}
decodeDocumentJson : Json.Decoder Document
decodeDocumentJson =
    Json.field "type" Json.string
        |> Json.andThen
            (\docType ->
                Json.at [ "data", docType ] (Json.dict decodeDocumentField)
            )
        |> Json.map Document


decodeDocumentField : Json.Decoder DocumentField
decodeDocumentField =
    let
        decodeOnType typeStr =
            case typeStr of
                "SliceZone" ->
                    Json.map SliceZone (Json.field "value" decodeSliceZone)

                _ ->
                    Json.map Field decodeField
    in
    Json.field "type" Json.string
        |> Json.andThen decodeOnType


decodeField : Json.Decoder Field
decodeField =
    let
        decodeOnType typeStr =
            case typeStr of
                "Text" ->
                    Json.map Text (Json.field "value" Json.string)

                "Select" ->
                    Json.map Select (Json.field "value" Json.string)

                "Color" ->
                    Json.map Color (Json.field "value" Json.string)

                "Number" ->
                    Json.map Number (Json.field "value" Json.float)

                "Date" ->
                    Json.map Date (Json.field "value" decodeDate)

                "Image" ->
                    Json.map Image (Json.field "value" decodeImageViews)

                "StructuredText" ->
                    Json.map StructuredTextField (Json.field "value" decodeStructuredText)

                "Link.document" ->
                    Json.map Link decodeLink

                "Link.web" ->
                    Json.map Link decodeLink

                "Group" ->
                    Json.map Groups (Json.field "value" (Json.list (Json.dict (Json.lazy (\_ -> decodeField)))))

                _ ->
                    Json.fail ("Unknown document field type: " ++ typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


decodeDate : Json.Decoder Date.Date
decodeDate =
    Json.string
        |> Json.andThen
            (\str ->
                case Date.fromString str of
                    Ok date ->
                        Json.succeed date

                    Err msg ->
                        Json.fail msg
            )


{-| Decode Html.a `DocumentReference` from JSON.
-}
decodeDocumentReferenceJson : Json.Decoder DocumentReference
decodeDocumentReferenceJson =
    Json.decode DocumentReference
        |> Json.required "id" Json.string
        |> Json.optional "uid" (Json.maybe Json.string) Nothing
        |> Json.required "slug" Json.string
        |> Json.required "tags" (Json.list Json.string)
        |> Json.required "type" Json.string


{-| Decode some `StructuredText`.
-}
decodeStructuredText : Json.Decoder StructuredText
decodeStructuredText =
    Json.map StructuredText (Json.list decodeStructuredTextBlock)


{-| Decode an `ImageField`.
-}
decodeImageViews : Json.Decoder ImageViews
decodeImageViews =
    Json.decode ImageViews
        |> Json.required "main" decodeImageView
        |> Json.required "views" (Json.dict decodeImageView)


decodeImageView : Json.Decoder ImageView
decodeImageView =
    Json.decode ImageView
        |> Json.required "alt" (Json.nullable Json.string)
        |> Json.required "copyright" (Json.nullable Json.string)
        |> Json.required "url" Json.string
        |> Json.required "dimensions" decodeImageDimensions


decodeImageDimensions : Json.Decoder ImageDimensions
decodeImageDimensions =
    Json.decode ImageDimensions
        |> Json.required "width" Json.int
        |> Json.required "height" Json.int


decodeStructuredTextBlock : Json.Decoder StructuredTextBlock
decodeStructuredTextBlock =
    let
        decodeOnType typeStr =
            case typeStr of
                "heading1" ->
                    Json.map Heading1 decodeBlock

                "heading2" ->
                    Json.map Heading2 decodeBlock

                "heading3" ->
                    Json.map Heading3 decodeBlock

                "paragraph" ->
                    Json.map Paragraph decodeBlock

                "list-item" ->
                    Json.map ListItem decodeBlock

                "image" ->
                    Json.map SImage decodeImageView

                "embed" ->
                    Json.map SEmbed (Json.field "oembed" decodeEmbed)

                _ ->
                    Json.fail ("Unknown structured field type: " ++ toString typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


decodeBlock : Json.Decoder Block
decodeBlock =
    Json.decode Block
        |> Json.required "text" Json.string
        |> Json.required "spans" (Json.list decodeSpan)
        |> Json.optional "label" (Json.maybe Json.string) Nothing


decodeSpan : Json.Decoder Span
decodeSpan =
    Json.decode Span
        |> Json.required "start" Json.int
        |> Json.required "end" Json.int
        |> Json.custom decodeSpanType


decodeSpanType : Json.Decoder SpanElement
decodeSpanType =
    let
        decodeOnType typeStr =
            case typeStr of
                "Html.em" ->
                    Json.succeed Em

                "Html.strong" ->
                    Json.succeed Strong

                "hyperlink" ->
                    Json.map Hyperlink (Json.field "data" decodeLink)

                _ ->
                    Json.fail ("Unknown span type: " ++ typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


{-| Decode an `Embed` field.
-}
decodeEmbed : Json.Decoder Embed
decodeEmbed =
    let
        decodeOnType typeStr =
            case typeStr of
                "video" ->
                    Json.map EVideo decodeEmbedVideo

                "rich" ->
                    Json.map ERich decodeEmbedRich

                _ ->
                    Json.fail ("Unknown embed type: " ++ typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


decodeEmbedVideo : Json.Decoder EmbedVideo
decodeEmbedVideo =
    Json.decode EmbedVideo
        |> Json.required "author_name" Json.string
        |> Json.required "author_url" Json.string
        |> Json.required "embed_url" Json.string
        |> Json.required "height" Json.int
        |> Json.required "html" Json.string
        |> Json.required "provider_name" Json.string
        |> Json.required "provider_url" Json.string
        |> Json.required "thumbnail_height" Json.int
        |> Json.required "thumbnail_url" Json.string
        |> Json.required "thumbnail_width" Json.int
        |> Json.required "title" Json.string
        |> Json.required "version" Json.string
        |> Json.required "width" Json.int


decodeEmbedRich : Json.Decoder EmbedRich
decodeEmbedRich =
    Json.decode EmbedRich
        |> Json.required "author_name" Json.string
        |> Json.required "author_url" Json.string
        |> Json.required "cache_age" Json.string
        |> Json.required "embed_url" Json.string
        |> Json.required "height" (Json.maybe Json.int)
        |> Json.required "html" Json.string
        |> Json.required "provider_name" Json.string
        |> Json.required "provider_url" Json.string
        |> Json.required "title" Json.string
        |> Json.required "url" Json.string
        |> Json.required "version" Json.string
        |> Json.required "width" Json.int


{-| Decode Html.a `Link`.
-}
decodeLink : Json.Decoder Link
decodeLink =
    let
        decodeOnType typeStr =
            case typeStr of
                "Link.document" ->
                    Json.decode DocumentLink
                        |> Json.requiredAt [ "value", "document" ] decodeDocumentReferenceJson
                        |> Json.requiredAt [ "value", "isBroken" ] Json.bool

                "Link.web" ->
                    Json.decode WebLink
                        |> Json.requiredAt [ "value", "url" ] Json.string

                _ ->
                    Json.fail ("Unknown link type: " ++ typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


decodeSliceZone : Json.Decoder SliceZone
decodeSliceZone =
    Json.list (Json.lazy (\_ -> decodeSlice))


decodeSlice : Json.Decoder Slice
decodeSlice =
    Json.decode Slice
        |> Json.optional "slice_label" (Json.maybe Json.string) Nothing
        |> Json.required "slice_type" Json.string
        |> Json.custom decodeSliceContent


decodeSliceContent : Json.Decoder SliceContentVersion
decodeSliceContent =
    Json.oneOf
        [ Json.field "value" (Json.lazy (\_ -> decodeField))
            |> Json.map SliceContentField
        , let
            miniDocument =
                Json.dict (Json.lazy (\_ -> decodeField))
          in
          Json.decode SliceContent
            |> Json.required "non-repeat" miniDocument
            |> Json.required "repeat" (Json.list miniDocument)
        ]
