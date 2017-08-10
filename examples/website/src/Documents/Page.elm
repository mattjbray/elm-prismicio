module Documents.Page exposing (..)

import Documents.Homepage exposing (BodySlice, bodySliceZone)
import Prismic.Decode exposing (decode)
import Prismic.Document exposing (Decoder, sliceZone)


type alias Page =
    { body : List BodySlice
    }


decodePage : Decoder Page
decodePage =
    decode Page
        |> sliceZone "body" bodySliceZone
