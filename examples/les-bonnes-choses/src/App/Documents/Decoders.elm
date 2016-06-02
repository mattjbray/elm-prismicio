module App.Documents.Decoders exposing (..)

import App.Documents.Types exposing (..)
import Json.Decode exposing (..)
import Prismic.Decoders exposing (..)


decodeArticle : Decoder Article
decodeArticle =
    succeed Article
        |: at [ "id" ] string
        |: at [ "data", "article", "content", "value" ] decodeStructuredText
        |: at [ "data", "article", "image", "value" ] decodeImageField
        |: at [ "data", "article", "short_lede", "value" ] decodeStructuredText
        |: at [ "data", "article", "title", "value" ] decodeStructuredText


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


decodeCategories : Decoder (List Category)
decodeCategories =
    let
        strToCategory str =
            case str of
                "Pie" ->
                    Just Pie

                "Macaron" ->
                    Just Macaron

                "Cupcake" ->
                    Just Cupcake

                _ ->
                    Nothing
    in
        (list string)
            `andThen` (succeed << List.filterMap strToCategory)


decodeProduct : Decoder Product
decodeProduct =
    (succeed Product
        |: at [ "id" ] string
        |: at [ "slugs" ] (list string)
        |: maybe (at [ "data", "product", "allergens", "value" ] string)
        |: at [ "data", "product", "color", "value" ] string
        |: at [ "data", "product", "description", "value" ] decodeStructuredText
        |: maybeWithDefault [] (at [ "data", "product", "flavour" ] (list ("value" := string)))
        |: maybeWithDefault [] (at [ "data", "product", "gallery" ] (list ("value" := decodeImageField)))
        |: at [ "data", "product", "image", "value" ] decodeImageField
        |: at [ "data", "product", "name", "value" ] decodeStructuredText
        |: at [ "data", "product", "price", "value" ] float
        |: maybeWithDefault [] (at [ "data", "product", "related" ] (list decodeLink))
        |: at [ "data", "product", "short_lede", "value" ] decodeStructuredText
        |: maybe (at [ "data", "product", "testimonial_author", "value" ] decodeStructuredText)
        |: maybe (at [ "data", "product", "testimonial_quote", "value" ] decodeStructuredText)
        |: at [ "tags" ] (list string)
        |: at [ "tags" ] decodeCategories
    )


decodeSelection : Decoder Selection
decodeSelection =
    succeed Selection
        |: at [ "id" ] string
        |: at [ "slugs" ] (list string)
        |: at [ "tags" ] (list string)
        |: at [ "data", "selection", "name", "value" ] decodeStructuredText
        |: at [ "data", "selection", "catcher_image", "value" ] decodeImageField
        |: at [ "data", "selection", "description", "value" ] decodeStructuredText
        |: at [ "data", "selection", "image", "value" ] decodeImageField
        |: at [ "data", "selection", "price", "value" ] float
        |: at [ "data", "selection", "product" ] (list decodeLink)
        |: at [ "data", "selection", "short_lede", "value" ] decodeStructuredText


decodeStore : Decoder Store
decodeStore =
    succeed Store
        |: at [ "id" ] string
        |: at [ "slugs" ] (list string)
        |: at [ "tags" ] (list string)
        |: at [ "data", "store", "address", "value" ] string
        |: at [ "data", "store", "city", "value" ] string
        |: at [ "data", "store", "zipcode", "value" ] string
        |: at [ "data", "store", "country", "value" ] string
        |: at [ "data", "store", "description", "value" ] decodeStructuredText
        |: at [ "data", "store", "name", "value" ] decodeStructuredText
        |: at [ "data", "store", "image", "value" ] decodeImageField
        |: at [ "data", "store", "monday" ] (list ("value" := string))
        |: at [ "data", "store", "tuesday" ] (list ("value" := string))
        |: at [ "data", "store", "wednesday" ] (list ("value" := string))
        |: at [ "data", "store", "thursday" ] (list ("value" := string))
        |: at [ "data", "store", "friday" ] (list ("value" := string))
        |: at [ "data", "store", "saturday" ] (list ("value" := string))
        |: at [ "data", "store", "sunday" ] (list ("value" := string))
