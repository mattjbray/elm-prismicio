module Documents.Menu exposing (..)

import Prismic.Document
    exposing
        ( Decoder
        , decode
        , required
        )
import Prismic.Document.Field as Field
    exposing
        ( Link
        , StructuredText
        , group
        , link
        , structuredText
        , text
        )


type alias Menu =
    { title : StructuredText
    , links : List MenuLink
    }


type alias MenuLink =
    { label : String
    , link : Link
    }


decodeMenu : Decoder Menu
decodeMenu =
    decode Menu
        |> required "title" structuredText
        |> required "menuLinks" (group decodeMenuLink)


decodeMenuLink : Field.GroupDecoder MenuLink
decodeMenuLink =
    Field.decode MenuLink
        |> Field.required "label" text
        |> Field.required "link" link
