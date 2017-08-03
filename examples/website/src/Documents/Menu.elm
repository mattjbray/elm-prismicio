module Documents.Menu exposing (..)

import Prismic.Document
    exposing
        ( Decoder
        , Link
        , StructuredText
        , decode
        , group
        , link
        , required
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


decodeMenuLink : Decoder MenuLink
decodeMenuLink =
    decode MenuLink
        |> required "label" text
        |> required "link" link
