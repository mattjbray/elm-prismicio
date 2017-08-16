module Documents.Menu exposing (..)

import Prismic
    exposing
        ( Decoder
        , Document
        , custom
        , decode
        , group
        , required
        )
import Prismic.Document.Field as Field
    exposing
        ( Field
        , Link
        , StructuredText
        , link
        , structuredText
        , text
        )
import Prismic.Document.Group as Group exposing (Group)


type alias Menu =
    { id : String
    , title : StructuredText
    , links : List MenuLink
    }


type alias MenuLink =
    { label : String
    , link : Link
    }


decodeMenu : Decoder Document Menu
decodeMenu =
    decode Menu
        |> custom Prismic.id
        |> required "title" structuredText
        |> custom (group "menuLinks" decodeMenuLink)


decodeMenuLink : Decoder Group MenuLink
decodeMenuLink =
    decode MenuLink
        |> Group.required "label" text
        |> Group.required "link" link
