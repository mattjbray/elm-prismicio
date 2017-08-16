module Documents.Page exposing (..)

import Documents.Homepage exposing (BodySlice, bodySliceZone)
import Prismic
    exposing
        ( Decoder
        , Document
        , map
        , sliceZone
        )


type alias Page =
    { body : List BodySlice
    }


decodePage : Decoder Document Page
decodePage =
    sliceZone "body" bodySliceZone
        |> map Page
