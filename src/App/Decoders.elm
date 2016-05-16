module App.Decoders exposing (..)

import App.Types exposing (..)
import Json.Decode exposing (..)
import Prismic.Decoders exposing (..)

decodeMyDocument : Decoder MyDocument
decodeMyDocument =
    let
        decodeOnType typeStr =
            case typeStr of
                "article" ->
                    object1 ArticleDoc decodeArticle

                "job-offer" ->
                    object1 JobOfferDoc decodeJobOffer

                "blog-post" ->
                    object1 BlogPostDoc decodeBlogPost

                _ ->
                    object1 Default decodeDefaultDocType
    in
        ("type" := string) `andThen` decodeOnType


decodeArticle : Decoder Article
decodeArticle =
    at [ "data", "article" ]
        (succeed Article
            |: at [ "content", "value" ] decodeStructuredText
            |: at [ "image", "value" ] decodeImageField
            |: at [ "short_lede", "value" ] decodeStructuredText
            |: at [ "title", "value" ] decodeStructuredText
        )


decodeJobOffer : Decoder JobOffer
decodeJobOffer =
    at [ "data", "job-offer" ]
        (succeed JobOffer
            |: at [ "name", "value" ] decodeStructuredText
            |: maybe (at [ "contract_type", "value" ] string)
            |: maybe (at [ "service", "value" ] string)
            |: at [ "job_description", "value" ] decodeStructuredText
            |: at [ "profile", "value" ] decodeStructuredText
            |: at [ "location" ] (list decodeLink)
        )


decodeBlogPost : Decoder BlogPost
decodeBlogPost =
    let
        decodeAllowComments str =
            case str of
                "Yes" ->
                    succeed True

                "No" ->
                    succeed False

                _ ->
                    fail ("Unknown allow_comments value: " ++ str)
    in
        at [ "data", "blog-post" ]
            (succeed BlogPost
                |: at [ "body", "value" ] decodeStructuredText
                |: at [ "author", "value" ] string
                |: at [ "category", "value" ] string
                |: at [ "date", "value" ] string
                |: at [ "shortlede", "value" ] decodeStructuredText
                |: at [ "relatedpost" ] (list decodeLink)
                |: at [ "relatedproduct" ] (list decodeLink)
                |: at [ "allow_comments", "value" ] (string `andThen` decodeAllowComments)
            )
