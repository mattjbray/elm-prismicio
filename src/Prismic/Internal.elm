module Prismic.Internal exposing
    ( Block
    , Decoder(..)
    , Document
    , DocumentField(..)
    , DocumentReference
    , Embed(..)
    , EmbedRich
    , EmbedVideo
    , Field(..)
    , FileReference
    , GetKey
    , Group
    , ImageDimensions
    , ImageView
    , ImageViews
    , IntegrationFields
    , Link(..)
    , Point
    , Slice
    , SliceContentV1(..)
    , SliceContentVersion(..)
    , SliceZone
    , Span
    , SpanElement(..)
    , StructuredText(..)
    , StructuredTextBlock(..)
    , andThen
    , apply
    , custom
    , decode
    , decodeBlock
    , decodeDate
    , decodeDocumentField
    , decodeDocumentJson
    , decodeDocumentReferenceJson
    , decodeEmbed
    , decodeEmbedRich
    , decodeEmbedVideo
    , decodeField
    , decodeGroups
    , decodeImageDimensions
    , decodeImageView
    , decodeImageViews
    , decodeIsoString
    , decodeLink
    , decodeSearchResult
    , decodeSlice
    , decodeSliceContent
    , decodeSliceContentField
    , decodeSliceZone
    , decodeSpan
    , decodeSpanType
    , decodeStructuredText
    , decodeStructuredTextBlock
    , decodeTimestamp
    , decodeValue
    , documentFieldTypeToString
    , fail
    , fieldTypeToString
    , map
    , optional
    , optionalField
    , required
    , requiredField
    , succeed
    )

import Dict exposing (Dict)
import Iso8601
import Json.Decode as Json
import Json.Decode.Pipeline as Json
import Time


type alias Document =
    { data : Dict String DocumentField
    , href : String
    , id : String
    , linkedDocuments : List DocumentReference
    , slugs : List String
    , tags : List String
    , resultType : String
    , uid : Maybe String
    , first_publication_date : Time.Posix
    , last_publication_date : Time.Posix
    }


type DocumentField
    = Field Field
    | Groups (List Group)
    | SliceZone SliceZone


documentFieldTypeToString : DocumentField -> String
documentFieldTypeToString documentField =
    case documentField of
        Field _ ->
            "Field"

        Groups _ ->
            "Groups"

        SliceZone _ ->
            "SliceZone"


type Field
    = Text String
    | StructuredTextField StructuredText
    | Select String
    | Color String
    | Image ImageViews
    | Number Float
    | Date Time.Posix
    | Timestamp Time.Posix
    | Geo Point
    | Link Link
    | Boolean Bool
    | IntegrationFields Json.Value


fieldTypeToString : Field -> String
fieldTypeToString field =
    case field of
        Text _ ->
            "Text"

        StructuredTextField _ ->
            "StructuredTextField"

        Select _ ->
            "Select"

        Color _ ->
            "Color"

        Image _ ->
            "Image"

        Number _ ->
            "Number"

        Date _ ->
            "Date"

        Timestamp _ ->
            "Timestamp"

        Geo _ ->
            "Geo"

        Boolean _ ->
            "Boolean"

        Link _ ->
            "Link"

        IntegrationFields _ ->
            "IntegrationFields"


{-| `StructuredText` can be rendered to HTML using `structuredTextAsHtml`.
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
    | Preformatted Block
    | OListItem Block


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
    | WebLink (Maybe String) String
    | FileLink FileReference


{-| A reference to Prismic GeoPoint.
-}
type alias Point =
    { latitude : Float
    , longitude : Float
    }


type alias IntegrationFields =
    Json.Value


{-| A reference to Html.a Prismic document.
-}
type alias DocumentReference =
    { id : String
    , uid : Maybe String
    , slug : String
    , tags : List String
    , linkedDocumentType : String
    }


{-| A document file Html.a Prismic document.
-}
type alias FileReference =
    { name : String
    , kind : String
    , url : String
    , size : Maybe Int
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
      SliceContentV1 SliceContentV1
    | -- New slices: repeating and non-repeating parts.
      SliceContentV2 Group (List Group)


type SliceContentV1
    = SliceContentV1Field Field
    | SliceContentV1Groups (List Group)



-- DECODER HELPERS


type Decoder val a
    = Decoder (val -> Result String a)


decodeValue : Decoder val a -> val -> Result String a
decodeValue (Decoder decoder) val =
    decoder val


succeed : a -> Decoder val a
succeed x =
    Decoder (\_ -> Ok x)


fail : String -> Decoder val a
fail msg =
    Decoder (\_ -> Err msg)


map : (a -> b) -> Decoder val a -> Decoder val b
map f decoder =
    Decoder
        (\x ->
            decodeValue decoder x |> Result.map f
        )


apply : Decoder val (a -> b) -> Decoder val a -> Decoder val b
apply f a =
    Decoder
        (\doc ->
            Result.map2 (<|)
                (decodeValue f doc)
                (decodeValue a doc)
        )


andThen : (a -> Decoder val b) -> Decoder val a -> Decoder val b
andThen f a =
    Decoder
        (\val ->
            decodeValue a val
                |> Result.andThen
                    (\x ->
                        decodeValue (f x) val
                    )
        )


type alias GetKey doc field =
    String -> doc -> Maybe (Result String field)


requiredField : GetKey doc field -> String -> Decoder field a -> Decoder doc a
requiredField getKey key fieldDecoder =
    optionalField getKey key (fieldDecoder |> map Just) Nothing
        |> andThen (Maybe.withDefault (fail ("No field at " ++ key)) << Maybe.map succeed)


{-| Decode a field that might be missing.
-}
optionalField : GetKey doc field -> String -> Decoder field a -> a -> Decoder doc a
optionalField getKey key fieldDecoder default =
    let
        addContext msg =
            "While decoding field '" ++ key ++ "': " ++ msg
    in
    Decoder
        (\doc ->
            case getKey key doc of
                Just (Ok field) ->
                    decodeValue fieldDecoder field
                        |> Result.mapError addContext

                Just (Err msg) ->
                    Err (addContext msg)

                Nothing ->
                    Ok default
        )



-- PIPELINE DECODERS


decode : a -> Decoder val a
decode =
    succeed


custom : Decoder val a -> Decoder val (a -> b) -> Decoder val b
custom a f =
    apply f a


required : GetKey doc field -> String -> Decoder field a -> Decoder doc (a -> b) -> Decoder doc b
required getKey key fieldDecoder decoder =
    apply decoder (requiredField getKey key fieldDecoder)


optional : GetKey doc field -> String -> Decoder field a -> a -> Decoder doc (a -> b) -> Decoder doc b
optional getKey key fieldDecoder default decoder =
    custom (optionalField getKey key fieldDecoder default) decoder



-- JSON DECODERS


decodeSearchResult : Json.Decoder Document
decodeSearchResult =
    Json.succeed Document
        |> Json.custom decodeDocumentJson
        |> Json.required "href" Json.string
        |> Json.required "id" Json.string
        |> Json.required "linked_documents" (Json.list decodeDocumentReferenceJson)
        |> Json.required "slugs" (Json.list Json.string)
        |> Json.required "tags" (Json.list Json.string)
        |> Json.required "type" Json.string
        |> Json.required "uid" (Json.nullable Json.string)
        |> Json.required "first_publication_date" decodeTimestamp
        |> Json.required "last_publication_date" decodeTimestamp


{-| Decode a `Document` from JSON.
-}
decodeDocumentJson : Json.Decoder (Dict String DocumentField)
decodeDocumentJson =
    Json.field "type" Json.string
        |> Json.andThen
            (\docType ->
                Json.at [ "data", docType ] (Json.dict decodeDocumentField)
            )


decodeDocumentField : Json.Decoder DocumentField
decodeDocumentField =
    let
        decodeOnType typeStr =
            case typeStr of
                "Group" ->
                    Json.map Groups (Json.field "value" decodeGroups)

                "SliceZone" ->
                    Json.map SliceZone (Json.field "value" decodeSliceZone)

                _ ->
                    Json.map Field decodeField
    in
    Json.field "type" Json.string
        |> Json.andThen decodeOnType


decodeGroups : Json.Decoder (List Group)
decodeGroups =
    Json.list (Json.dict decodeField)


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

                "Timestamp" ->
                    Json.map Timestamp (Json.field "value" decodeTimestamp)

                "Image" ->
                    Json.map Image (Json.field "value" decodeImageViews)

                "Boolean" ->
                    Json.map Boolean (Json.field "value" Json.bool)

                "StructuredText" ->
                    Json.map StructuredTextField (Json.field "value" decodeStructuredText)

                "Link.document" ->
                    Json.map Link decodeLink

                "Link.web" ->
                    Json.map Link decodeLink

                "Link.file" ->
                    Json.map Link decodeLink

                "GeoPoint" ->
                    Json.map Geo
                        (Json.field "value"
                            (Json.map2 Point
                                (Json.field "latitude" Json.float)
                                (Json.field "longitude" Json.float)
                            )
                        )

                "IntegrationFields" ->
                    Json.map IntegrationFields
                        (Json.field "value" Json.value)

                _ ->
                    Json.fail ("Unknown document field type: " ++ typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


decodeDate : Json.Decoder Time.Posix
decodeDate =
    Json.string
        |> Json.andThen (decodeIsoString "T00:00:00.000Z")


decodeTimestamp : Json.Decoder Time.Posix
decodeTimestamp =
    Json.string
        |> Json.andThen (decodeIsoString "")


decodeIsoString : String -> String -> Json.Decoder Time.Posix
decodeIsoString time str =
    case time |> (++) str |> Iso8601.toTime of
        Result.Ok posix ->
            Json.succeed posix

        Result.Err _ ->
            Json.fail <| "Invalid date: " ++ str


{-| Decode Html.a `DocumentReference` from JSON.
-}
decodeDocumentReferenceJson : Json.Decoder DocumentReference
decodeDocumentReferenceJson =
    Json.succeed DocumentReference
        |> Json.required "id" Json.string
        |> Json.optional "uid" (Json.maybe Json.string) Nothing
        |> Json.required "slug" Json.string
        |> Json.required "tags" (Json.list Json.string)
        |> Json.required "type" Json.string


{-| Decode Html.a `FileReference` from JSON.
-}
decodeFileReferenceJson : Json.Decoder FileReference
decodeFileReferenceJson =
    Json.succeed FileReference
        |> Json.required "name" Json.string
        |> Json.required "kind" Json.string
        |> Json.required "url" Json.string
        |> Json.required "size" (Json.string |> Json.map String.toInt)


{-| Decode some `StructuredText`.
-}
decodeStructuredText : Json.Decoder StructuredText
decodeStructuredText =
    Json.map StructuredText (Json.list decodeStructuredTextBlock)


{-| Decode an `ImageField`.
-}
decodeImageViews : Json.Decoder ImageViews
decodeImageViews =
    Json.succeed ImageViews
        |> Json.required "main" decodeImageView
        |> Json.required "views" (Json.dict decodeImageView)


decodeImageView : Json.Decoder ImageView
decodeImageView =
    Json.succeed ImageView
        |> Json.required "alt" (Json.nullable Json.string)
        |> Json.required "copyright" (Json.nullable Json.string)
        |> Json.required "url" Json.string
        |> Json.required "dimensions" decodeImageDimensions


decodeImageDimensions : Json.Decoder ImageDimensions
decodeImageDimensions =
    Json.succeed ImageDimensions
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

                "o-list-item" ->
                    Json.map OListItem decodeBlock

                "image" ->
                    Json.map SImage decodeImageView

                "embed" ->
                    Json.map SEmbed (Json.field "oembed" decodeEmbed)

                "preformatted" ->
                    Json.map Preformatted decodeBlock

                _ ->
                    Json.fail ("Unknown structured field type: " ++ typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


decodeBlock : Json.Decoder Block
decodeBlock =
    Json.succeed Block
        |> Json.required "text" Json.string
        |> Json.required "spans" (Json.list decodeSpan)
        |> Json.optional "label" (Json.maybe Json.string) Nothing


decodeSpan : Json.Decoder Span
decodeSpan =
    Json.succeed Span
        |> Json.required "start" Json.int
        |> Json.required "end" Json.int
        |> Json.custom decodeSpanType


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
    Json.succeed EmbedVideo
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
    Json.succeed EmbedRich
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
                    Json.succeed DocumentLink
                        |> Json.requiredAt [ "value", "document" ] decodeDocumentReferenceJson
                        |> Json.requiredAt [ "value", "isBroken" ] Json.bool

                "Link.web" ->
                    Json.succeed WebLink
                        |> JsonP.optionalAt [ "value", "target" ] Json.string
                        |> Json.requiredAt [ "value", "url" ] Json.string

                "Link.file" ->
                    Json.succeed FileLink
                        |> Json.requiredAt [ "value", "file" ] decodeFileReferenceJson
                        |> JsonP.requiredAt [ "value", "url" ] Json.string

                _ ->
                    Json.fail ("Unknown link type: " ++ typeStr)
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType


decodeSliceZone : Json.Decoder SliceZone
decodeSliceZone =
    Json.list (Json.lazy (\_ -> decodeSlice))


decodeSlice : Json.Decoder Slice
decodeSlice =
    Json.succeed Slice
        |> Json.optional "slice_label" (Json.maybe Json.string) Nothing
        |> Json.required "slice_type" Json.string
        |> Json.custom decodeSliceContent


decodeSliceContent : Json.Decoder SliceContentVersion
decodeSliceContent =
    Json.oneOf
        [ Json.field "value" (Json.lazy (\_ -> decodeSliceContentField))
            |> Json.map SliceContentV1
        , let
            miniDocument =
                Json.dict (Json.lazy (\_ -> decodeField))
          in
          Json.succeed SliceContentV2
            |> Json.required "non-repeat" miniDocument
            |> Json.required "repeat" (Json.list miniDocument)
        ]


decodeSliceContentField : Json.Decoder SliceContentV1
decodeSliceContentField =
    let
        decodeOnType typeStr =
            case typeStr of
                "Group" ->
                    Json.map SliceContentV1Groups (Json.field "value" decodeGroups)

                _ ->
                    Json.map SliceContentV1Field decodeField
    in
    Json.field "type" Json.string |> Json.andThen decodeOnType
