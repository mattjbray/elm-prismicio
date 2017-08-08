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
        , required
        , slice
        , sliceV1
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
    | GalleryV2 GalleryWithTitle
    | SingleRepeat (List StructuredText)


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


type alias GalleryWithTitle =
    { title : StructuredText
    , groups : List GalleryGroup
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
        [ sliceV1 "heading" Heading structuredText
        , labelledSlice "textSection" TextSection structuredText
        , sliceV1 "highlight" Highlight (group decodeHighlightGroup)
        , sliceV1 "fullWidthImage" FullWidthImage image
        , sliceV1 "gallery" Gallery (group decodeGalleryGroup)
        , slice "new_image_gallery"
            (\title groups -> GalleryV2 (GalleryWithTitle title groups))
            (field "title" structuredText)
            decodeGalleryGroup
        , slice "single_repeat"
            (\() texts -> SingleRepeat texts)
            (decode ())
            (field "title" structuredText)
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
