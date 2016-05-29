module App.Documents.Decoders exposing (..)

import App.Documents.Types exposing (..)
import Json.Decode exposing (..)
import Prismic.Decoders exposing (..)


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
        succeed BlogPost
            |: at [ "id" ] string
            |: at [ "slugs" ] (list string)
            |: at [ "data", "blog-post", "body", "value" ] decodeStructuredText
            |: at [ "data", "blog-post", "author", "value" ] string
            |: at [ "data", "blog-post", "category", "value" ] string
            |: at [ "data", "blog-post", "date", "value" ] string
            |: at [ "data", "blog-post", "shortlede", "value" ] decodeStructuredText
            |: at [ "data", "blog-post", "relatedpost" ] (list decodeLink)
            |: at [ "data", "blog-post", "relatedproduct" ] (list decodeLink)
            |: at [ "data", "blog-post", "allow_comments", "value" ] (string `andThen` decodeAllowComments)


decodeProduct : Decoder Product
decodeProduct =
    at [ "data", "product" ]
        (succeed Product
            |: at [ "allergens", "value" ] string
            |: at [ "color", "value" ] string
            |: at [ "description", "value" ] decodeStructuredText
            |: at [ "flavour" ] (list ("value" := string))
            |: maybeWithDefault [] (at [ "gallery" ] (list ("value" := decodeImageField)))
            |: at [ "image", "value" ] decodeImageField
            |: at [ "name", "value" ] decodeStructuredText
            |: at [ "price", "value" ] float
            |: at [ "related" ] (list decodeLink)
            |: at [ "short_lede", "value" ] decodeStructuredText
            |: maybe (at [ "testimonial_author", "value" ] decodeStructuredText)
            |: maybe (at [ "testimonial_quote", "value" ] decodeStructuredText)
        )
