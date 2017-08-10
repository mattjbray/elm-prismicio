module Documents.Page exposing (..)

import Documents.Homepage exposing (BodySlice, bodySliceZone)
import Prismic.Document exposing (Decoder, sliceZone)
import Prismic.Document.Decoders exposing (decode)


type alias Page =
    { body : List BodySlice
    }


decodePage : Decoder Page
decodePage =
    decode Page
        |> sliceZone "body" bodySliceZone
