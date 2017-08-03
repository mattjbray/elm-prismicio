module Documents.Homepage exposing (..)

import Prismic.Document
    exposing
        ( Decoder
        , ImageViews
        , Link
        , StructuredText
        , decode
        , group
        , image
        , labelledSlice
        , link
        , optional
        , required
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
        |> required "title" structuredText
        |> required "tagline" structuredText
        |> required "buttonText" text
        |> required "buttonLink" link
        |> required "backgroundImage" image
        |> required "body" bodySliceZone


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
        |> required "title" structuredText
        |> required "headline" structuredText
        |> required "image" image
        |> optional "link" link
        |> optional "linkText" text


decodeGalleryGroup : Decoder GalleryGroup
decodeGalleryGroup =
    decode GalleryGroup
        |> required "description" structuredText
        |> required "image" image
