module Documents.Page exposing (..)

import Documents.Homepage exposing (BodySlice, bodySliceZone)
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


type alias Page =
    { body : List BodySlice
    }


decodePage : Decoder Page
decodePage =
    decode Page
        |> required "body" bodySliceZone
