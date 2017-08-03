module Documents.Page exposing (..)

import Documents.Homepage exposing (BodySlice, bodySliceZone)
import Prismic.Document exposing (Decoder, decode, required)


type alias Page =
    { body : List BodySlice
    }


decodePage : Decoder Page
decodePage =
    decode Page
        |> required "body" bodySliceZone
