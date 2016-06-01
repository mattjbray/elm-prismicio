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
                    object1 BlogPostF Documents.decodeBlogPost

                "product" ->
                    object1 ProductF Documents.decodeProduct

                "selection" ->
                    object1 SelectionF Documents.decodeSelection

                _ ->
                    fail ("Unexpected document type: " ++ typeStr)
    in
        ("type" := string) `andThen` decodeOnType
