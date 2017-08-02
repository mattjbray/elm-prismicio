module Documents.Page exposing (..)

import Documents.Homepage exposing (BodySlice, bodySliceZone)
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


type alias Page =
    { body : List BodySlice
    }


decodePage : Decoder Page
decodePage =
    decode Page
        |> field "body" bodySliceZone
