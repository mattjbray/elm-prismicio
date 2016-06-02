module App.Site.Search.Results.Decoders exposing (..)

import App.Documents.Decoders as Documents
import App.Site.Search.Results.Types exposing (..)
import Json.Decode exposing (..)


decodeArticleR : Decoder ArticleR
decodeArticleR =
    let
        decodeOnType typeStr =
            case typeStr of
                "article" ->
                    object1 ArticleR Documents.decodeArticle

                "blog-post" ->
                    object1 BlogPostR Documents.decodeBlogPost

                "store" ->
                    object1 StoreR Documents.decodeStore

                _ ->
                    fail ("Unexpected document type: " ++ typeStr)
    in
        ("type" := string) `andThen` decodeOnType



decodeProductR : Decoder ProductR
decodeProductR =
    let
        decodeOnType typeStr =
            case typeStr of
                "product" ->
                    object1 ProductR Documents.decodeProduct

                "selection" ->
                    object1 SelectionR Documents.decodeSelection

                _ ->
                    fail ("Unexpected document type: " ++ typeStr)
    in
        ("type" := string) `andThen` decodeOnType
