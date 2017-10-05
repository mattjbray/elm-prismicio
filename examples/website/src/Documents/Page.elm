module Documents.Page exposing (..)

import Documents.Homepage exposing (BodySlice, bodySliceZone)
import Prismic
    exposing
        ( Decoder
        , Document
        , sliceZone
        )


type alias Page =
    { body : List BodySlice
    }


decodePage : Decoder Document Page
decodePage =
    Prismic.decode Page
        |> sliceZone "body" bodySliceZone
