module Documents.Homepage exposing (..)

import Prismic
    exposing
        ( Decoder
        , Document
        , custom
        , decode
        , group
        , map
        , optional
        , required
        , sliceZone
        )
import Prismic.Field as Field
    exposing
        ( ImageViews
        , Link
        , StructuredText
        , image
        , link
        , structuredText
        , text
        )
import Prismic.Group as Group exposing (Group)
import Prismic.Slice as Slice
    exposing
        ( Slice
        , labelledV1Slice
        , slice
        , v1Slice
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


decodeHomepage : Decoder Document Homepage
decodeHomepage =
    decode Homepage
        |> required "title" structuredText
        |> required "tagline" structuredText
        |> required "buttonText" text
        |> required "buttonLink" link
        |> required "backgroundImage" image
        |> sliceZone "body" bodySliceZone


bodySliceZone : Decoder Slice BodySlice
bodySliceZone =
    Slice.oneOf
        [ v1Slice "heading" Heading (Slice.field structuredText)
        , labelledV1Slice "textSection" TextSection (Slice.field structuredText)
        , v1Slice "highlight" Highlight (Slice.group decodeHighlightGroup)
        , v1Slice "fullWidthImage" FullWidthImage (Slice.field image)
        , v1Slice "gallery" Gallery (Slice.group decodeGalleryGroup)
        , slice "new_image_gallery"
            (Group.field "title" structuredText)
            decodeGalleryGroup
            |> Prismic.map
                (\( title, groups ) -> GalleryV2 (GalleryWithTitle title groups))
        , slice "single_repeat"
            (decode ())
            (Group.field "title" structuredText)
            |> Prismic.map (\( _, texts ) -> SingleRepeat texts)
        ]


decodeHighlightGroup : Decoder Group HighlightGroup
decodeHighlightGroup =
    decode HighlightGroup
        |> Group.required "title" structuredText
        |> Group.required "headline" structuredText
        |> Group.required "image" image
        |> Group.optional "link" (map Just link) Nothing
        |> Group.optional "linkText" (map Just text) Nothing


decodeGalleryGroup : Decoder Group GalleryGroup
decodeGalleryGroup =
    decode GalleryGroup
        |> Group.required "description" structuredText
        |> Group.required "image" image
