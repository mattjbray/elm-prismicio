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
                    map ArticleR Documents.decodeArticle

                "blog-post" ->
                    map BlogPostR Documents.decodeBlogPost

                "store" ->
                    map StoreR Documents.decodeStore

                "job-offer" ->
                    map JobR Documents.decodeJobOffer

                _ ->
                    fail ("Unexpected document type: " ++ typeStr)
    in
        field "type" string |> andThen decodeOnType


decodeProductR : Decoder ProductR
decodeProductR =
    let
        decodeOnType typeStr =
            case typeStr of
                "product" ->
                    map ProductR Documents.decodeProduct

                "selection" ->
                    map SelectionR Documents.decodeSelection

                _ ->
                    fail ("Unexpected document type: " ++ typeStr)
    in
        field "type" string |> andThen decodeOnType
