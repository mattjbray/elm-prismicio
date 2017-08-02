module Documents.Menu exposing (..)

import Prismic.Document
    exposing
        ( Decoder
        , Link
        , StructuredText
        , decode
        , field
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
        |> field "title" structuredText
        |> field "menuLinks" (group decodeMenuLink)


decodeMenuLink : Decoder MenuLink
decodeMenuLink =
    decode MenuLink
        |> field "label" text
        |> field "link" link
