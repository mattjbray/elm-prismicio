module App.Site.Home.Decoders exposing (..)

import App.Documents.Decoders as Documents
import App.Site.Home.Types exposing (..)
import Json.Decode exposing (..)


decodeFeatured : Decoder Featured
decodeFeatured =
    let
        decodeOnType typeStr =
            case typeStr of
                "blog-post" ->
                    map BlogPostF Documents.decodeBlogPost

                "product" ->
                    map ProductF Documents.decodeProduct

                "selection" ->
                    map SelectionF Documents.decodeSelection

                _ ->
                    fail ("Unexpected document type: " ++ typeStr)
    in
        field "type" string |> andThen decodeOnType
