module Prismic.Decoders exposing (..)

import Json.Decode exposing (..)
import Prismic.Types exposing (..)


-- HELPERS


(|:) : Decoder (a -> b) -> Decoder a -> Decoder b
(|:) =
    object2 (<|)


maybeWithDefault : a -> Decoder a -> Decoder a
maybeWithDefault default decoder =
    maybe decoder `andThen` (succeed << (Maybe.withDefault default))


nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    oneOf
        [ null Nothing
        , map Just decoder
        ]


decodeRef : Decoder Ref
decodeRef =
    object1 Ref string


decodeUrl : Decoder Url
decodeUrl =
    object1 Url string



-- DECODERS


decodeApi : Decoder Api
decodeApi =
    succeed Api
        |: ("refs" := list decodeRefProperties)
        |: ("bookmarks" := dict string)
        |: ("types" := dict string)
        |: ("tags" := list string)
        |: ("version" := string)
        |: ("forms" := dict decodeForm)
        |: ("oauth_initiate" := string)
        |: ("oauth_token" := string)
        |: ("license" := string)
        |: ("experiments" := decodeExperiments)


decodeRefProperties : Decoder RefProperties
decodeRefProperties =
    succeed RefProperties
        |: ("id" := string)
        |: ("ref" := decodeRef)
        |: ("label" := string)
        |: (maybeWithDefault False ("isMasterRef" := bool))


decodeForm : Decoder Form
decodeForm =
    succeed Form
        |: ("method" := string)
        |: ("enctype" := string)
        |: ("action" := decodeUrl)
        |: ("fields" := dict decodeFormField)
        |: (maybe ("rel" := string))
        |: (maybe ("name" := string))


decodeFormField : Decoder FormField
decodeFormField =
    succeed FormField
        |: ("type" := decodeFieldType)
        |: ("multiple" := bool)
        |: (maybe ("default" := string))


decodeFieldType : Decoder FieldType
decodeFieldType =
    let
        decodeOnType str =
            case str of
                "String" ->
                    succeed String

                "Integer" ->
                    succeed Integer

                _ ->
                    fail ("Unknown field type: " ++ str)
    in
        string `andThen` decodeOnType


decodeExperiments : Decoder Experiments
decodeExperiments =
    succeed Experiments
        |: ("draft" := list string)
        |: ("running" := list string)


decodeResponse : Decoder docType -> Decoder (Response docType)
decodeResponse decodeDocType =
    succeed Response
        |: ("license" := string)
        |: ("next_page" := nullOr decodeUrl)
        |: ("page" := int)
        |: ("prev_page" := nullOr decodeUrl)
        |: ("results" := list (decodeSearchResult decodeDocType))
        |: ("results_per_page" := int)
        |: ("results_size" := int)
        |: ("total_pages" := int)
        |: ("total_results_size" := int)
        |: ("version" := string)


decodeDefaultDocType : Decoder DefaultDocType
decodeDefaultDocType =
    "data"
        := dict
            (dict
                (oneOf
                    [ object1 (\x -> [ x ]) decodeDocumentField
                    , list decodeDocumentField
                    ]
                )
            )


decodeSearchResult : Decoder docType -> Decoder (SearchResult docType)
decodeSearchResult decodeDocType =
    succeed SearchResult
        |: decodeDocType
        |: ("href" := decodeUrl)
        |: ("id" := string)
        |: ("linked_documents" := list decodeLinkedDocument)
        |: ("slugs" := list string)
        |: ("tags" := list string)
        |: ("type" := string)
        |: ("uid" := nullOr string)


decodeLinkedDocument : Decoder LinkedDocument
decodeLinkedDocument =
    succeed LinkedDocument
        |: ("id" := string)
        |: ("slug" := string)
        |: ("tags" := list string)
        |: ("type" := string)


decodeDocumentField : Decoder DocumentField
decodeDocumentField =
    let
        decodeOnType typeStr =
            case typeStr of
                "Text" ->
                    object1 Text ("value" := string)

                "Select" ->
                    object1 Select ("value" := string)

                "Color" ->
                    object1 Color ("value" := string)

                "Number" ->
                    object1 Number ("value" := float)

                "Date" ->
                    object1 Date ("value" := string)

                "Image" ->
                    object1 Image ("value" := decodeImageField)

                "StructuredText" ->
                    object1 StructuredText ("value" := decodeStructuredText)

                "Link.document" ->
                    object1 Link decodeLink

                "Link.web" ->
                    object1 Link decodeLink

                _ ->
                    fail ("Unknown document field type: " ++ typeStr)
    in
        ("type" := string) `andThen` decodeOnType


decodeStructuredText : Decoder StructuredText
decodeStructuredText =
    list decodeStructuredTextField


decodeImageField : Decoder ImageField
decodeImageField =
    succeed ImageField
        |: ("main" := decodeImageProperties)
        |: ("views" := (dict decodeImageProperties))


decodeImageProperties : Decoder ImageProperties
decodeImageProperties =
    succeed ImageProperties
        |: ("alt" := nullOr string)
        |: ("copyright" := nullOr string)
        |: ("url" := decodeUrl)
        |: ("dimensions" := decodeImageDimensions)


decodeImageDimensions : Decoder ImageDimensions
decodeImageDimensions =
    succeed ImageDimensions
        |: ("width" := int)
        |: ("height" := int)


decodeStructuredTextField : Decoder StructuredTextField
decodeStructuredTextField =
    let
        decodeOnType typeStr =
            case typeStr of
                "heading1" ->
                    object1 SSimple (decodeSimpleStructuredTextField Heading1)

                "heading2" ->
                    object1 SSimple (decodeSimpleStructuredTextField Heading2)

                "heading3" ->
                    object1 SSimple (decodeSimpleStructuredTextField Heading3)

                "paragraph" ->
                    object1 SSimple (decodeSimpleStructuredTextField Paragraph)

                "list-item" ->
                    object1 SSimple (decodeSimpleStructuredTextField ListItem)

                "image" ->
                    object1 SImage (decodeImageProperties)

                "embed" ->
                    object1 SEmbed ("oembed" := decodeEmbedProperties)

                _ ->
                    fail ("Unknown structured field type: " ++ toString typeStr)
    in
        ("type" := string) `andThen` decodeOnType


decodeSimpleStructuredTextField : SimpleStructuredTextType -> Decoder SimpleStructuredTextField
decodeSimpleStructuredTextField tag =
    succeed (SimpleStructuredTextField tag)
        |: ("text" := string)
        |: ("spans" := list decodeSpan)


decodeSpan : Decoder Span
decodeSpan =
    succeed Span
        |: ("start" := int)
        |: ("end" := int)
        |: decodeSpanType


decodeSpanType : Decoder SpanType
decodeSpanType =
    let
        decodeOnType typeStr =
            case typeStr of
                "em" ->
                    succeed Em

                "strong" ->
                    succeed Strong

                "hyperlink" ->
                    object1 Hyperlink ("data" := decodeLink)

                _ ->
                    fail ("Unknown span type: " ++ typeStr)
    in
        ("type" := string) `andThen` decodeOnType


decodeEmbedProperties : Decoder EmbedProperties
decodeEmbedProperties =
    let
        decodeOnType typeStr =
            case typeStr of
                "video" ->
                    object1 EmbedVideo decodeEmbedVideoProperties

                "rich" ->
                    object1 EmbedRich decodeEmbedRichProperties

                _ ->
                    fail ("Unknown embed type: " ++ typeStr)
    in
        ("type" := string) `andThen` decodeOnType


decodeEmbedVideoProperties : Decoder EmbedVideoProperties
decodeEmbedVideoProperties =
    succeed EmbedVideoProperties
        |: ("author_name" := string)
        |: ("author_url" := decodeUrl)
        |: ("embed_url" := decodeUrl)
        |: ("height" := int)
        |: ("html" := string)
        |: ("provider_name" := string)
        |: ("provider_url" := decodeUrl)
        |: ("thumbnail_height" := int)
        |: ("thumbnail_url" := decodeUrl)
        |: ("thumbnail_width" := int)
        |: ("title" := string)
        |: ("version" := string)
        |: ("width" := int)


decodeEmbedRichProperties : Decoder EmbedRichProperties
decodeEmbedRichProperties =
    succeed EmbedRichProperties
        |: ("author_name" := string)
        |: ("author_url" := decodeUrl)
        |: ("cache_age" := string)
        |: ("embed_url" := decodeUrl)
        |: ("height" := maybe int)
        |: ("html" := string)
        |: ("provider_name" := string)
        |: ("provider_url" := decodeUrl)
        |: ("title" := string)
        |: ("url" := decodeUrl)
        |: ("version" := string)
        |: ("width" := int)


decodeLink : Decoder Link
decodeLink =
    let
        decodeOnType typeStr =
            case typeStr of
                "Link.document" ->
                    succeed DocumentLink
                        |: (at [ "value", "document" ] decodeLinkedDocument)
                        |: (at [ "value", "isBroken" ] bool)

                "Link.web" ->
                    succeed WebLink
                        |: (at [ "value", "url" ] decodeUrl)

                _ ->
                    fail ("Unknown link type: " ++ typeStr)
    in
        ("type" := string) `andThen` decodeOnType
