module Documents.Homepage exposing (..)

import Prismic.Document
    exposing
        ( Decoder
        , ImageViews
        , StructuredText
        , Link
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
    , backgroundImage : ImageViews
    , body : List BodySlice
    }


type BodySlice
    = TextSection (Maybe String) StructuredText
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
        |> field "backgroundImage" image
        |> field "body"
            (sliceZone
                [ labelledSlice "textSection" TextSection structuredText
                , slice "highlight" Highlight (group decodeHighlightGroup)
                , slice "fullWidthImage" FullWidthImage image
                , slice "gallery" Gallery (group decodeGalleryGroup)
                ]
            )


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
