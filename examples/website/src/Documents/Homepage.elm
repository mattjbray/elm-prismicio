module Documents.Homepage exposing (..)

import Prismic.Document
    exposing
        ( Decoder
        , ImageViews
        , Link
        , StructuredText
        , decode
        , field
        , group
        , image
        , labelledSlice
        , link
        , optional
        , slice
        , sliceZone
        , structuredText
        , text
        )


type alias Homepage =
    { title : StructuredText
    , tagline : StructuredText
    , buttonText : String
    , buttonLink : Link
    , backgroundImage : ImageViews
    , body : List BodySlice
    }


type BodySlice
    = Heading StructuredText
    | TextSection (Maybe String) StructuredText
    | Highlight (List HighlightGroup)
    | FullWidthImage ImageViews
    | Gallery (List GalleryGroup)


type alias HighlightGroup =
    { title : StructuredText
    , headline : StructuredText
    , image : ImageViews
    , link : Maybe Link
    , linkText : Maybe String
    }


type alias GalleryGroup =
    { description : StructuredText
    , image : ImageViews
    }


decodeHomepage : Decoder Homepage
decodeHomepage =
    decode Homepage
        |> field "title" structuredText
        |> field "tagline" structuredText
        |> field "buttonText" text
        |> field "buttonLink" link
        |> field "backgroundImage" image
        |> field "body" bodySliceZone


bodySliceZone : Prismic.Document.FieldDecoder (List BodySlice)
bodySliceZone =
    sliceZone
        [ slice "heading" Heading structuredText
        , labelledSlice "textSection" TextSection structuredText
        , slice "highlight" Highlight (group decodeHighlightGroup)
        , slice "fullWidthImage" FullWidthImage image
        , slice "gallery" Gallery (group decodeGalleryGroup)
        ]


decodeHighlightGroup : Decoder HighlightGroup
decodeHighlightGroup =
    decode HighlightGroup
        |> field "title" structuredText
        |> field "headline" structuredText
        |> field "image" image
        |> optional "link" link
        |> optional "linkText" text


decodeGalleryGroup : Decoder GalleryGroup
decodeGalleryGroup =
    decode GalleryGroup
        |> field "description" structuredText
        |> field "image" image
