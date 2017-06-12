module Prismic.Api.V1 exposing (..)

import Prismic.Api exposing (..)
import Prismic.Url exposing (..)
import Prismic.Document exposing (..)

import Json.Decode as Json
import Json.Decode.Pipeline exposing (required, optional, custom, requiredAt)
import Json.Decode.Pipeline as Json



-- DECODER HELPERS


maybeWithDefault : a -> Json.Decoder a -> Json.Decoder a
maybeWithDefault default decoder =
    Json.maybe decoder |> Json.andThen (Json.succeed << Maybe.withDefault default)


decodeRef : Json.Decoder Ref
decodeRef =
    Json.map Ref Json.string


decodeUrl : Json.Decoder Url
decodeUrl =
    Json.map Url Json.string



-- DECODERS


decodeApi : Json.Decoder Api
decodeApi =
    Json.decode Api
        |> required "refs" (Json.list decodeRefProperties)
        |> required "bookmarks" (Json.dict Json.string)
        |> required "types" (Json.dict Json.string)
        |> required "tags" (Json.list Json.string)
        |> required "version" Json.string
        |> required "forms" (Json.dict decodeForm)
        |> required "oauth_initiate" Json.string
        |> required "oauth_token" Json.string
        |> required "license" Json.string
        |> required "experiments" decodeExperiments


decodeRefProperties : Json.Decoder RefProperties
decodeRefProperties =
    Json.decode RefProperties
        |> required "id" Json.string
        |> required "ref" decodeRef
        |> required "label" Json.string
        |> optional "isMasterRef" Json.bool False


decodeForm : Json.Decoder Form
decodeForm =
    Json.decode Form
        |> required "method" Json.string
        |> required "enctype" Json.string
        |> required "action" decodeUrl
        |> required "fields" (Json.dict decodeFormField)
        |> optional "rel" (Json.maybe Json.string) Nothing
        |> optional "name" (Json.maybe Json.string) Nothing


decodeFormField : Json.Decoder FormField
decodeFormField =
    Json.decode FormField
        |> required "type" decodeFieldType
        |> required "multiple" Json.bool
        |> optional "default" (Json.maybe Json.string) Nothing


decodeFieldType : Json.Decoder FieldType
decodeFieldType =
    let
        decodeOnType str =
            case str of
                "String" ->
                    Json.succeed String

                "Integer" ->
                    Json.succeed Integer

                _ ->
                    Json.fail ("Unknown field type: " ++ str)
    in
    Json.string |> Json.andThen decodeOnType


decodeExperiments : Json.Decoder Experiments
decodeExperiments =
    Json.decode Experiments
        |> required "draft" (Json.list Json.string)
        |> required "running" (Json.list Json.string)


decodeResponse : Json.Decoder (Response Document)
decodeResponse =
    Json.decode Response
        |> required "license" Json.string
        |> required "next_page" (Json.nullable decodeUrl)
        |> required "page" Json.int
        |> required "prev_page" (Json.nullable decodeUrl)
        |> required "results" (Json.list decodeSearchResult)
        |> required "results_per_page" Json.int
        |> required "results_size" Json.int
        |> required "total_pages" Json.int
        |> required "total_results_size" Json.int
        |> required "version" Json.string


{-| Decode a result to a `Document`.
-}
decodeDocument : Json.Decoder Document
decodeDocument =
    Json.field "type" Json.string
        |> Json.andThen
            (\docType ->
                Json.at [ "data", docType ] (Json.dict decodeDocumentField)
            )
        |> Json.map Document


decodeSearchResult : Json.Decoder (SearchResult Document)
decodeSearchResult =
    Json.decode SearchResult
        |> custom decodeDocument
        |> required "href" decodeUrl
        |> required "id" Json.string
        |> required "linked_documents" (Json.list decodeDocumentReference)
        |> required "slugs" (Json.list Json.string)
        |> required "tags" (Json.list Json.string)
        |> required "type" Json.string
        |> required "uid" (Json.nullable Json.string)


decodeDocumentReference : Json.Decoder DocumentReference
decodeDocumentReference =
    Json.decode DocumentReference
        |> required "id" Json.string
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
                    Json.map StructuredText (Json.field "value" decodeStructuredText)

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
    Json.list decodeStructuredTextBlock


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
                        |> requiredAt [ "value", "document" ] decodeDocumentReference
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
        |> optional "slice_label" (Json.maybe Json.string) Nothing
        |> required "slice_type" Json.string
        |> required "value" decodeDocumentField
