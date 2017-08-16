module Examples.Group exposing (..)

import Prismic exposing (Decoder, Document)
import Prismic.Field as Field
import Prismic.Group as Group exposing (Group)


type alias MyDoc =
    { albums : List Album }


type alias Album =
    { title : String
    , cover : Field.ImageViews
    }


albumDecoder : Decoder Group Album
albumDecoder =
    Prismic.decode Album
        |> Group.required "title" Field.text
        |> Group.required "cover" Field.image


myDocDecoder : Decoder Document MyDoc
myDocDecoder =
    Prismic.decode MyDoc
        |> Prismic.custom (Prismic.group "albums" albumDecoder)
