module Documents.Menu exposing (..)

import Prismic.Decode exposing (custom, decode)
import Prismic.Document as Document
    exposing
        ( Decoder
        , group
        , required
        )
import Prismic.Document.Field as Field
    exposing
        ( Link
        , StructuredText
        , link
        , structuredText
        , text
        )
import Prismic.Document.Group as Group


type alias Menu =
    { id : String
    , title : StructuredText
    , links : List MenuLink
    }


type alias MenuLink =
    { label : String
    , link : Link
    }


decodeMenu : Decoder Menu
decodeMenu =
    decode Menu
        |> custom Document.id
        |> required "title" structuredText
        |> custom (group "menuLinks" decodeMenuLink)


decodeMenuLink : Group.Decoder MenuLink
decodeMenuLink =
    decode MenuLink
        |> Group.required "label" text
        |> Group.required "link" link
