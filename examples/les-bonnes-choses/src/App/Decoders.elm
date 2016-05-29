module App.Decoders exposing (..)

import App.Documents.Decoders as Documents
import App.Types exposing (..)
import Json.Decode exposing (..)
import Prismic.Decoders exposing (..)


decodeMyDocument : Decoder MyDocument
decodeMyDocument =
    let
        decodeOnType typeStr =
            case typeStr of
                "article" ->
                    object1 ArticleDoc Documents.decodeArticle

                "job-offer" ->
                    object1 JobOfferDoc Documents.decodeJobOffer

                "blog-post" ->
                    object1 BlogPostDoc Documents.decodeBlogPost

                _ ->
                    object1 Default decodeDefaultDocType
    in
        ("type" := string) `andThen` decodeOnType
