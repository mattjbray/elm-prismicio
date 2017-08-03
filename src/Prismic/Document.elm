module Prismic.Document
    exposing
        ( Decoder
        , Document
        , DocumentReference
        , Embed(..)
        , EmbedRich
        , EmbedVideo
        , FieldDecoder
        , ImageDimensions
        , ImageView
        , ImageViews
        , Link(DocumentLink, WebLink)
        , SliceDecoder
        , StructuredText
        , StructuredTextBlock
        , decode
        , decodeDocument
        , decodeDocumentJson
        , decodeDocumentReferenceJson
        , defaultLinkResolver
        , field
        , getFirstImage
        , getFirstParagraph
        , getText
        , getTexts
        , getTitle
        , group
        , image
        , labelledSlice
        , link
        , map
        , optional
        , slice
        , sliceZone
        , structuredText
        , structuredTextAsHtml
        , structuredTextBlockAsHtml
        , text
        )

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

@docs Decoder, decode, map, FieldDecoder, field, optional, text, structuredText, image, link, SliceDecoder, sliceZone, slice, labelledSlice, group


## Viewing documents

@docs structuredTextAsHtml, structuredTextBlockAsHtml
@docs defaultLinkResolver


### `StructuredText` helpers

@docs getTitle, getFirstImage, getFirstParagraph, getText, getTexts


## Internal

JSON decoders used internally by `elm-prismicio`.

@docs decodeDocument, decodeDocumentJson, decodeDocumentReferenceJson

-}

import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, div, em, h1, h2, h3, img, li, p, strong, ul)
import Html.Attributes exposing (class, href, property, src)
import Json.Decode as Json
import Json.Decode.Pipeline as Json exposing (custom, required, requiredAt)
import Json.Encode
import Prismic.Url exposing (Url(Url), decodeUrl)
import Result.Extra as Result


{-| Holds the Prismic document.

You will decode this into your own document type by passing a `Decoder MyDoc` to
`submit`.

-}
type Document
    = Document (Dict String DocumentField)


{-| A field in the `Document`.
-}
type DocumentField
    = Text String
    | StructuredTextField StructuredText
    | Select String
    | Color String
    | Image ImageViews
    | Number Float
    | Date String
    | Link Link
    | SliceZone SliceZone
    | Groups (List Group)



--  DOCUMENT DECODERS


{-| A value that knows how to decode Documents.

Construct a `Decoder` to pass to `submit`.

-}
type Decoder a
    = Decoder (Document -> Result String a)


{-| Decodes a field in a `Document`.
-}
type FieldDecoder a
    = FieldDecoder (DocumentField -> Result String a)


{-| Begin a decoding pipeline.

    type alias MyDoc =
        { title : StructuredText }

    myDocDecoder : Decoder MyDoc
    myDocDecoder =
        decode MyDoc
            |> field "title" structuredText

-}
decode : a -> Decoder a
decode doc =
    Decoder (\_ -> Ok doc)


fail : String -> Decoder a
fail msg =
    Decoder (\_ -> Err msg)


{-| Internal: Run a `Decoder` against a `Document`.
-}
decodeDocument : Decoder a -> Document -> Result String a
decodeDocument (Decoder decoder) doc =
    decoder doc


{-| Transform a decoder.
-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decoder) =
    Decoder
        (\doc ->
            decoder doc |> Result.map f
        )


apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply (Decoder f) (Decoder a) =
    Decoder
        (\doc ->
            case ( f doc, a doc ) of
                ( Ok g, Ok x ) ->
                    Ok (g x)

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err
        )


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f (Decoder a) =
    Decoder
        (\doc ->
            case a doc of
                Ok x ->
                    let
                        (Decoder g) =
                            f x
                    in
                    g doc

                Err err ->
                    Err err
        )


{-| Decode a field.
-}
field : String -> FieldDecoder a -> Decoder (a -> b) -> Decoder b
field key valDecoder decoder =
    apply decoder
        (fieldKey key valDecoder
            |> andThen
                (\res ->
                    case res of
                        Just x ->
                            decode x

                        Nothing ->
                            fail ("No field at " ++ key)
                )
        )


fieldKey : String -> FieldDecoder a -> Decoder (Maybe a)
fieldKey key (FieldDecoder fieldDecoder) =
    Decoder
        (\(Document doc) ->
            case Dict.get key doc of
                Just field ->
                    fieldDecoder field
                        |> Result.map Just
                        |> Result.mapError
                            (\msg ->
                                "While decoding field '" ++ key ++ "': " ++ msg
                            )

                Nothing ->
                    Ok Nothing
        )


{-| Decode a field that might be missing.
-}
optional : String -> FieldDecoder a -> Decoder (Maybe a -> b) -> Decoder b
optional key valDecoder decoder =
    apply decoder (fieldKey key valDecoder)


{-| Decode a Text field.
-}
text : FieldDecoder String
text =
    FieldDecoder
        (\field ->
            case field of
                Text text ->
                    Ok text

                _ ->
                    Err ("Expected a Text field, but got '" ++ toString field ++ "'.")
        )


{-| Decode a StructuredText field.
-}
structuredText : FieldDecoder StructuredText
structuredText =
    FieldDecoder
        (\field ->
            case field of
                StructuredTextField x ->
                    Ok x

                _ ->
                    Err ("Expected a StructuredText field, but got '" ++ toString field ++ "'.")
        )


{-| Decode an Image field.
-}
image : FieldDecoder ImageViews
image =
    FieldDecoder
        (\field ->
            case field of
                Image x ->
                    Ok x

                _ ->
                    Err ("Expected an Image field, but got '" ++ toString field ++ "'.")
        )


{-| Decode a Link field.
-}
link : FieldDecoder Link
link =
    FieldDecoder
        (\field ->
            case field of
                Link x ->
                    Ok x

                _ ->
                    Err ("Expected a Link field, but got '" ++ toString field ++ "'.")
        )


{-| Decodes a `Slice` field.
-}
type SliceDecoder a
    = SliceDecoder (Slice -> Result String a)


oneOf : List (SliceDecoder a) -> Slice -> Result String a
oneOf sliceDecoders slice =
    let
        go decoders errors =
            case decoders of
                [] ->
                    Err
                        ("No slices matched: \n* "
                            ++ String.join "\n* " errors
                        )

                (SliceDecoder decoder) :: rest ->
                    case decoder slice of
                        Ok x ->
                            Ok x

                        Err err ->
                            go rest (err :: errors)
    in
    go sliceDecoders []


{-| Decode a slice in a slice zone. The tagger is also passed the slice label.

TODO: custom label decoders?

-}
labelledSlice : String -> (Maybe String -> a -> b) -> FieldDecoder a -> SliceDecoder b
labelledSlice sliceType tagger (FieldDecoder fieldDecoder) =
    SliceDecoder
        (\slice ->
            if sliceType == slice.sliceType then
                fieldDecoder slice.sliceField
                    |> Result.map (tagger slice.sliceLabel)
                    |> Result.mapError
                        (\msg -> "While decoding slice with type '" ++ slice.sliceType ++ "': " ++ msg)
            else
                Err ("Expected slice with type '" ++ sliceType ++ "' but got '" ++ slice.sliceType ++ "'.")
        )


{-| Decode a slice in a slice zone.
-}
slice : String -> (a -> b) -> FieldDecoder a -> SliceDecoder b
slice sliceType tagger fieldDecoder =
    labelledSlice sliceType (\_ -> tagger) fieldDecoder


{-| Decode a SliceZone.

Pass this function a list of possible elements that can appear in the Slice.

    type alias MyDoc =
        { section : Section }

    type Section
        = MyContent StructuredText
        | MyImage ImageViews

    myDocDecoder : Decode MyDoc
    myDocDecoder =
        decode MyDoc
            |> field "section"
                (sliceZone
                    [ slice "theContent" MyContent structuredText
                    , slice "theImage" MyImage image
                    ]
                )

-}
sliceZone : List (SliceDecoder a) -> FieldDecoder (List a)
sliceZone decoders =
    FieldDecoder
        (\field ->
            case field of
                SliceZone slices ->
                    slices
                        |> List.map (oneOf decoders)
                        |> Result.collect

                _ ->
                    Err "Expected a SliceZone field."
        )


{-| Decode a group.

Groups are essentially Documents, so you pass `group` a Document `Decoder`.

Here is an example with a slice containing groups:

    type alias MyDoc =
        { slices : List Slice }

    type Slice
        = SAlbum Album
        | SBook Book

    type alias Album =
        { title : String
        , cover : ImageViews
        }

    type alias Book =
        { title : String
        , blurb : StructuredText
        }

    albumDecoder : Decoder Album
    albumDecoder =
        decode Album
            |> field "title" text
            |> field "cover" image

    bookDecoder : Decoder Book
    bookDecoder =
        decode Book
            |> field "title" text
            |> field "blurb" structuredText

    myDocDecoder : Decoder MyDoc
    myDocDecoder =
        decode MyDoc
            |> field "slices"
                (sliceZone
                    [ slice "album" (group albumDecoder)
                    , slice "book" (group bookDecoder)
                    ]
                )

-}
group : Decoder a -> FieldDecoder (List a)
group decoder =
    FieldDecoder
        (\field ->
            case field of
                Groups groups ->
                    groups
                        |> List.map Document
                        |> List.map (decodeDocument decoder)
                        |> Result.collect

                _ ->
                    Err ("Expected a Group field, but got '" ++ toString field ++ "'.")
        )


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


{-| Properties for a single image view.
-}
type alias ImageView =
    { alt : Maybe String
    , copyright : Maybe String
    , url : Url
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
    , authorUrl : Url
    , embedUrl : Url
    , height : Int
    , html : String
    , providerName : String
    , providerUrl : Url
    , thumbnailHeight : Int
    , thumbnailUrl : Url
    , thumbnailWidth : Int
    , title : String
    , version : String
    , width : Int
    }


{-| Rich embed elements.
-}
type alias EmbedRich =
    { authorName : String
    , authorUrl : Url
    , cacheAge : String
    , embedUrl : Url
    , height : Maybe Int
    , html : String
    , providerName : String
    , providerUrl : Url
    , title : String
    , url : Url
    , version : String
    , width : Int
    }


{-| Links to other documents or to the web.
-}
type Link
    = DocumentLink DocumentReference Bool
    | WebLink Url


{-| A reference to a Prismic document.
-}
type alias DocumentReference =
    { id : String
    , uid : Maybe String
    , slug : String
    , tags : List String
    , linkedDocumentType : String
    }


{-| Contains zero or more `Slice`s.
-}
type alias SliceZone =
    List Slice


{-| A Slice
-}
type alias Slice =
    { sliceLabel : Maybe String
    , sliceType : String
    , sliceField :
        DocumentField

    -- TODO: SliceField to exclude nested Slices?
    }


type alias Group =
    Dict String DocumentField


{-| Render some `StructuredText` as HTML.

You must supply a `linkResolver` to resolve any links in the `StructuredText`.
If you don't care about this, you can use the `defaultLinkResolver`.

-}
structuredTextAsHtml : (DocumentReference -> Url) -> StructuredText -> List (Html msg)
structuredTextAsHtml linkResolver (StructuredText blocks) =
    List.map (structuredTextBlockAsHtml linkResolver) blocks


{-| Render a single block of `StructuredText` as HTML.
-}
structuredTextBlockAsHtml : (DocumentReference -> Url) -> StructuredTextBlock -> Html msg
structuredTextBlockAsHtml linkResolver field =
    case field of
        SImage image ->
            imageAsHtml image

        SEmbed embed ->
            embedAsHtml embed

        Heading1 block ->
            blockAsHtml h1 linkResolver block

        Heading2 block ->
            blockAsHtml h2 linkResolver block

        Heading3 block ->
            blockAsHtml h3 linkResolver block

        Paragraph block ->
            blockAsHtml p linkResolver block

        ListItem block ->
            blockAsHtml
                (\attrs childs ->
                    ul [] [ li attrs childs ]
                )
                linkResolver
                block


blockAsHtml :
    (List (Attribute msg)
     -> List (Html msg)
     -> Html msg
    )
    -> (DocumentReference -> Url)
    -> Block
    -> Html msg
blockAsHtml el linkResolver field =
    let
        spanEl span =
            case span.spanElement of
                Em ->
                    em []

                Strong ->
                    strong []

                Hyperlink link ->
                    linkAsHtmlWith linkResolver link

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


imageAsHtml : ImageView -> Html msg
imageAsHtml image =
    let
        (Url urlStr) =
            image.url
    in
    img [ src urlStr ] []


embedAsHtml : Embed -> Html msg
embedAsHtml embed =
    case embed of
        EVideo props ->
            div [ property "innerHTML" (Json.Encode.string props.html) ] []

        ERich props ->
            div [ property "innerHTML" (Json.Encode.string props.html) ] []


linkAsHtml : (DocumentReference -> Url) -> Link -> Html msg
linkAsHtml linkResolver link =
    case link of
        DocumentLink linkedDoc isBroken ->
            let
                (Url url) =
                    linkResolver linkedDoc
            in
            a [ href url ] [ Html.text (toString linkedDoc.slug) ]

        WebLink (Url url) ->
            a [ href url ] [ Html.text url ]


linkAsHtmlWith : (DocumentReference -> Url) -> Link -> List (Html msg) -> Html msg
linkAsHtmlWith linkResolver link childs =
    case link of
        DocumentLink linkedDoc isBroken ->
            let
                (Url url) =
                    linkResolver linkedDoc
            in
            a [ href url ] childs

        WebLink (Url url) ->
            a [ href url ] childs


{-| Provide a default URL for `linkedDocuments`:

    Url "documents/doc.id/doc.slug"

-}
defaultLinkResolver : DocumentReference -> Url
defaultLinkResolver linkedDoc =
    Url (String.join "/" [ "documents", linkedDoc.id, linkedDoc.slug ])


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


{-| Decode a `DocumentReference` from JSON.
-}
decodeDocumentReferenceJson : Json.Decoder DocumentReference
decodeDocumentReferenceJson =
    Json.decode DocumentReference
        |> required "id" Json.string
        |> Json.optional "uid" (Json.maybe Json.string) Nothing
        |> required "slug" Json.string
        |> required "tags" (Json.list Json.string)
        |> required "type" Json.string


decodeDocumentField : Json.Decoder DocumentField
decodeDocumentField =
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
                    Json.map Date (Json.field "value" Json.string)

                "Image" ->
                    Json.map Image (Json.field "value" decodeImageViews)

                "StructuredText" ->
                    Json.map StructuredTextField (Json.field "value" decodeStructuredText)

                "Link.document" ->
                    Json.map Link decodeLink

                "Link.web" ->
                    Json.map Link decodeLink

                "SliceZone" ->
                    Json.map SliceZone (Json.field "value" decodeSliceZone)

                "Group" ->
                    Json.map Groups (Json.field "value" (Json.list (Json.dict (Json.lazy (\_ -> decodeDocumentField)))))

                _ ->
                    Json.fail ("Unknown document field type: " ++ typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


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
        |> required "main" decodeImageView
        |> required "views" (Json.dict decodeImageView)


decodeImageView : Json.Decoder ImageView
decodeImageView =
    Json.decode ImageView
        |> required "alt" (Json.nullable Json.string)
        |> required "copyright" (Json.nullable Json.string)
        |> required "url" decodeUrl
        |> required "dimensions" decodeImageDimensions


decodeImageDimensions : Json.Decoder ImageDimensions
decodeImageDimensions =
    Json.decode ImageDimensions
        |> required "width" Json.int
        |> required "height" Json.int


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
        |> required "text" Json.string
        |> required "spans" (Json.list decodeSpan)
        |> Json.optional "label" (Json.maybe Json.string) Nothing


decodeSpan : Json.Decoder Span
decodeSpan =
    Json.decode Span
        |> required "start" Json.int
        |> required "end" Json.int
        |> custom decodeSpanType


decodeSpanType : Json.Decoder SpanElement
decodeSpanType =
    let
        decodeOnType typeStr =
            case typeStr of
                "em" ->
                    Json.succeed Em

                "strong" ->
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
        |> required "author_name" Json.string
        |> required "author_url" decodeUrl
        |> required "embed_url" decodeUrl
        |> required "height" Json.int
        |> required "html" Json.string
        |> required "provider_name" Json.string
        |> required "provider_url" decodeUrl
        |> required "thumbnail_height" Json.int
        |> required "thumbnail_url" decodeUrl
        |> required "thumbnail_width" Json.int
        |> required "title" Json.string
        |> required "version" Json.string
        |> required "width" Json.int


decodeEmbedRich : Json.Decoder EmbedRich
decodeEmbedRich =
    Json.decode EmbedRich
        |> required "author_name" Json.string
        |> required "author_url" decodeUrl
        |> required "cache_age" Json.string
        |> required "embed_url" decodeUrl
        |> required "height" (Json.maybe Json.int)
        |> required "html" Json.string
        |> required "provider_name" Json.string
        |> required "provider_url" decodeUrl
        |> required "title" Json.string
        |> required "url" decodeUrl
        |> required "version" Json.string
        |> required "width" Json.int


{-| Decode a `Link`.
-}
decodeLink : Json.Decoder Link
decodeLink =
    let
        decodeOnType typeStr =
            case typeStr of
                "Link.document" ->
                    Json.decode DocumentLink
                        |> requiredAt [ "value", "document" ] decodeDocumentReferenceJson
                        |> requiredAt [ "value", "isBroken" ] Json.bool

                "Link.web" ->
                    Json.decode WebLink
                        |> requiredAt [ "value", "url" ] decodeUrl

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
        |> required "slice_type" Json.string
        |> required "value" decodeDocumentField
