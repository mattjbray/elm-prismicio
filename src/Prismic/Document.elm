module Prismic.Document
    exposing
        ( Block
        , Decoder
        , Document(..)
        , DocumentField(..)
        , DocumentReference
        , Embed(..)
        , EmbedRich
        , EmbedVideo
        , FieldDecoder
        , ImageDimensions
        , ImageView
        , ImageViews
        , Link(DocumentLink, WebLink)
        , Slice
        , SliceDecoder
        , SliceZone
        , Span
        , SpanElement(..)
        , StructuredText
        , StructuredTextBlock(..)
        , decode
        , decodeDocument
        , defaultLinkResolver
        , field
        , getFirstImage
        , getFirstParagraph
        , getText
        , getTexts
        , getTitle
        , group
        , image
        , map
        , slice
        , sliceZone
        , structuredText
        , structuredTextAsHtml
        , text
        )

{-|


## Documents

@docs Document, DocumentField


### Field types

You can create your own Elm types to represent your documents using the
following components.


#### Structured Text

@docs StructuredText, StructuredTextBlock, Block, Span, SpanElement


#### Images

@docs ImageViews, ImageView, ImageDimensions


#### Embeds

@docs Embed, EmbedRich, EmbedVideo


#### Links

@docs Link, DocumentReference


#### Slices

@docs SliceZone, Slice


## Decoding documents

@docs Decoder, decode, decodeDocument, map, FieldDecoder, field, text, structuredText, image, SliceDecoder, sliceZone, slice, group


## Viewing documents

@docs structuredTextAsHtml
@docs defaultLinkResolver


### `StructuredText` helpers

@docs getTitle, getFirstImage, getFirstParagraph, getText, getTexts

-}

import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, div, em, h1, h2, h3, img, li, p, strong, ul)
import Html.Attributes exposing (href, property, src)
import Json.Encode
import Prismic.Url exposing (Url(Url))
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
    | StructuredText StructuredText
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


{-| Decodes a `Document` field.
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


{-| Decode a field.
-}
field : String -> FieldDecoder a -> Decoder (a -> b) -> Decoder b
field key valDecoder decoder =
    apply decoder (fieldKey key valDecoder)


fieldKey : String -> FieldDecoder a -> Decoder a
fieldKey key (FieldDecoder fieldDecoder) =
    Decoder
        (\(Document doc) ->
            case Dict.get key doc of
                Just field ->
                    fieldDecoder field
                        |> Result.mapError (\msg -> "While decoding field '" ++ key ++ "': " ++ msg)

                Nothing ->
                    Err ("No field at " ++ key)
        )


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
                StructuredText x ->
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


{-| Decode a Slice field.
-}
slice : String -> (a -> b) -> FieldDecoder a -> SliceDecoder b
slice sliceType tagger (FieldDecoder fieldDecoder) =
    SliceDecoder
        (\slice ->
            if sliceType == slice.sliceType then
                fieldDecoder slice.sliceField
                    |> Result.map tagger
                    |> Result.mapError
                        (\msg -> "While decoding slice with type '" ++ slice.sliceType ++ "': " ++ msg)
            else
                Err ("Expected slice with type '" ++ sliceType ++ "' but got '" ++ slice.sliceType ++ "'.")
        )


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

An example of a slice containing groups:

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


{-| `StructuredText` is a list of `StructuredTextBlock`s.
-}
type alias StructuredText =
    List StructuredTextBlock


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
structuredTextAsHtml linkResolver =
    List.map (structuredTextBlockAsHtml linkResolver)


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
    el []
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
getTitle structuredText =
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
getFirstParagraph structuredText =
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
getFirstImage structuredText =
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
getTexts fields =
    fields
        |> List.map getText
        |> String.join " "
