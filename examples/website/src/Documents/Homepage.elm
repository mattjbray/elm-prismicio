module Documents.Homepage exposing (..)

import Prismic.Document
    exposing
        ( Decoder
        , decode
        , field
        , optional
        , required
        , sliceZone
        )
import Prismic.Document.Field as Field
    exposing
        ( ImageViews
        , Link
        , StructuredText
        , group
        , image
        , link
        , structuredText
        , text
        )
import Prismic.Document.Slice as Slice
    exposing
        ( labelledSlice
        , slice
        , sliceV1
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
        |> sliceZone "body" bodySliceZone


bodySliceZone : Slice.Decoder BodySlice
bodySliceZone =
    Slice.oneOf
        [ sliceV1 "heading" Heading structuredText
        , labelledSlice "textSection" TextSection structuredText
        , sliceV1 "highlight" Highlight (group decodeHighlightGroup)
        , sliceV1 "fullWidthImage" FullWidthImage image
        , sliceV1 "gallery" Gallery (group decodeGalleryGroup)
        , slice "new_image_gallery"
            (\title groups -> GalleryV2 (GalleryWithTitle title groups))
            (Field.field "title" structuredText)
            decodeGalleryGroup
        , slice "single_repeat"
            (\() texts -> SingleRepeat texts)
            (Field.decode ())
            (Field.field "title" structuredText)
        ]


decodeHighlightGroup : Field.GroupDecoder HighlightGroup
decodeHighlightGroup =
    Field.decode HighlightGroup
        |> Field.required "title" structuredText
        |> Field.required "headline" structuredText
        |> Field.required "image" image
        |> Field.optional "link" (Field.map Just link) Nothing
        |> Field.optional "linkText" (Field.map Just text) Nothing


decodeGalleryGroup : Field.GroupDecoder GalleryGroup
decodeGalleryGroup =
    Field.decode GalleryGroup
        |> Field.required "description" structuredText
        |> Field.required "image" image
