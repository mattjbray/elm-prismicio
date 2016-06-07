module App.Documents.Decoders exposing (..)

import App.Documents.Types exposing (..)
import Json.Decode exposing (..)
import Prismic as P


(|:) : Decoder (a -> b) -> Decoder a -> Decoder b
(|:) =
    object2 (<|)


maybeWithDefault : a -> Decoder a -> Decoder a
maybeWithDefault default decoder =
    maybe decoder `andThen` (succeed << (Maybe.withDefault default))


decodeArticle : Decoder Article
decodeArticle =
    succeed Article
        |: at [ "id" ] string
        |: at [ "data", "article", "content", "value" ] P.decodeStructuredText
        |: at [ "data", "article", "image", "value" ] P.decodeImageViews
        |: at [ "data", "article", "short_lede", "value" ] P.decodeStructuredText
        |: at [ "data", "article", "title", "value" ] P.decodeStructuredText


decodeJobOffer : Decoder JobOffer
decodeJobOffer =
    succeed JobOffer
        |: at [ "id" ] string
        |: at [ "slugs" ] (list string)
        |: at [ "tags" ] (list string)
        |: at [ "data", "job-offer", "name", "value" ] P.decodeStructuredText
        |: maybe (at [ "data", "job-offer", "contract_type", "value" ] string)
        |: maybe (at [ "data", "job-offer", "service", "value" ] string)
        |: at [ "data", "job-offer", "job_description", "value" ] P.decodeStructuredText
        |: at [ "data", "job-offer", "profile", "value" ] P.decodeStructuredText
        |: at [ "data", "job-offer", "location" ] (list P.decodeLink)


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
            |: at [ "data", "blog-post", "body", "value" ] P.decodeStructuredText
            |: at [ "data", "blog-post", "author", "value" ] string
            |: at [ "data", "blog-post", "category", "value" ] string
            |: at [ "data", "blog-post", "date", "value" ] string
            |: at [ "data", "blog-post", "shortlede", "value" ] P.decodeStructuredText
            |: at [ "data", "blog-post", "relatedpost" ] (list P.decodeLink)
            |: at [ "data", "blog-post", "relatedproduct" ] (list P.decodeLink)
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
        |: at [ "data", "product", "description", "value" ] P.decodeStructuredText
        |: maybeWithDefault [] (at [ "data", "product", "flavour" ] (list ("value" := string)))
        |: maybeWithDefault [] (at [ "data", "product", "gallery" ] (list ("value" := P.decodeImageViews)))
        |: at [ "data", "product", "image", "value" ] P.decodeImageViews
        |: at [ "data", "product", "name", "value" ] P.decodeStructuredText
        |: at [ "data", "product", "price", "value" ] float
        |: maybeWithDefault [] (at [ "data", "product", "related" ] (list P.decodeLink))
        |: at [ "data", "product", "short_lede", "value" ] P.decodeStructuredText
        |: maybe (at [ "data", "product", "testimonial_author", "value" ] P.decodeStructuredText)
        |: maybe (at [ "data", "product", "testimonial_quote", "value" ] P.decodeStructuredText)
        |: at [ "tags" ] (list string)
        |: at [ "tags" ] decodeCategories
    )


decodeSelection : Decoder Selection
decodeSelection =
    succeed Selection
        |: at [ "id" ] string
        |: at [ "slugs" ] (list string)
        |: at [ "tags" ] (list string)
        |: at [ "data", "selection", "name", "value" ] P.decodeStructuredText
        |: at [ "data", "selection", "catcher_image", "value" ] P.decodeImageViews
        |: at [ "data", "selection", "description", "value" ] P.decodeStructuredText
        |: at [ "data", "selection", "image", "value" ] P.decodeImageViews
        |: at [ "data", "selection", "price", "value" ] float
        |: at [ "data", "selection", "product" ] (list P.decodeLink)
        |: at [ "data", "selection", "short_lede", "value" ] P.decodeStructuredText


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
        |: at [ "data", "store", "description", "value" ] P.decodeStructuredText
        |: at [ "data", "store", "name", "value" ] P.decodeStructuredText
        |: at [ "data", "store", "image", "value" ] P.decodeImageViews
        |: at [ "data", "store", "monday" ] (list ("value" := string))
        |: at [ "data", "store", "tuesday" ] (list ("value" := string))
        |: at [ "data", "store", "wednesday" ] (list ("value" := string))
        |: at [ "data", "store", "thursday" ] (list ("value" := string))
        |: at [ "data", "store", "friday" ] (list ("value" := string))
        |: at [ "data", "store", "saturday" ] (list ("value" := string))
        |: at [ "data", "store", "sunday" ] (list ("value" := string))
