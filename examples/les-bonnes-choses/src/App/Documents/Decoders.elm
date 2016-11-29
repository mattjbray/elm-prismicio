module App.Documents.Decoders exposing (..)

import App.Documents.Types exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Prismic as P


decodeArticle : Decoder Article
decodeArticle =
    decode Article
        |> requiredAt [ "id" ] string
        |> requiredAt [ "data", "article", "content", "value" ] P.decodeStructuredText
        |> requiredAt [ "data", "article", "image", "value" ] P.decodeImageViews
        |> requiredAt [ "data", "article", "short_lede", "value" ] P.decodeStructuredText
        |> requiredAt [ "data", "article", "title", "value" ] P.decodeStructuredText


decodeJobOffer : Decoder JobOffer
decodeJobOffer =
    decode JobOffer
        |> requiredAt [ "id" ] string
        |> requiredAt [ "slugs" ] (list string)
        |> requiredAt [ "tags" ] (list string)
        |> requiredAt [ "data", "job-offer", "name", "value" ] P.decodeStructuredText
        |> optionalAt [ "data", "job-offer", "contract_type", "value" ] (maybe string) Nothing
        |> optionalAt [ "data", "job-offer", "service", "value" ] (maybe string) Nothing
        |> requiredAt [ "data", "job-offer", "job_description", "value" ] P.decodeStructuredText
        |> requiredAt [ "data", "job-offer", "profile", "value" ] P.decodeStructuredText
        |> requiredAt [ "data", "job-offer", "location" ] (list P.decodeLink)


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
        decode BlogPost
            |> requiredAt [ "id" ] string
            |> requiredAt [ "slugs" ] (list string)
            |> requiredAt [ "data", "blog-post", "body", "value" ] P.decodeStructuredText
            |> requiredAt [ "data", "blog-post", "author", "value" ] string
            |> requiredAt [ "data", "blog-post", "category", "value" ] string
            |> requiredAt [ "data", "blog-post", "date", "value" ] string
            |> requiredAt [ "data", "blog-post", "shortlede", "value" ] P.decodeStructuredText
            |> requiredAt [ "data", "blog-post", "relatedpost" ] (list P.decodeLink)
            |> requiredAt [ "data", "blog-post", "relatedproduct" ] (list P.decodeLink)
            |> requiredAt [ "data", "blog-post", "allow_comments", "value" ]
                (string |> andThen decodeAllowComments)


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
            |> andThen (succeed << List.filterMap strToCategory)


decodeProduct : Decoder Product
decodeProduct =
    succeed Product
        |> requiredAt [ "id" ] string
        |> requiredAt [ "slugs" ] (list string)
        |> optionalAt [ "data", "product", "allergens", "value" ]
            (maybe string)
            Nothing
        |> requiredAt [ "data", "product", "color", "value" ] string
        |> requiredAt [ "data", "product", "description", "value" ]
            P.decodeStructuredText
        |> optionalAt [ "data", "product", "flavour" ]
            (list (field "value" string))
            []
        |> optionalAt [ "data", "product", "gallery" ]
            (list (field "value" P.decodeImageViews))
            []
        |> requiredAt [ "data", "product", "image", "value" ] P.decodeImageViews
        |> requiredAt [ "data", "product", "name", "value" ] P.decodeStructuredText
        |> requiredAt [ "data", "product", "price", "value" ] float
        |> optionalAt [ "data", "product", "related" ] (list P.decodeLink) []
        |> requiredAt [ "data", "product", "short_lede", "value" ] P.decodeStructuredText
        |> optionalAt [ "data", "product", "testimonial_author", "value" ]
            (maybe P.decodeStructuredText)
            Nothing
        |> optionalAt [ "data", "product", "testimonial_quote", "value" ]
            (maybe P.decodeStructuredText)
            Nothing
        |> requiredAt [ "tags" ] (list string)
        |> requiredAt [ "tags" ] decodeCategories


decodeSelection : Decoder Selection
decodeSelection =
    decode Selection
        |> requiredAt [ "id" ] string
        |> requiredAt [ "slugs" ] (list string)
        |> requiredAt [ "tags" ] (list string)
        |> requiredAt [ "data", "selection", "name", "value" ] P.decodeStructuredText
        |> requiredAt [ "data", "selection", "catcher_image", "value" ] P.decodeImageViews
        |> requiredAt [ "data", "selection", "description", "value" ] P.decodeStructuredText
        |> requiredAt [ "data", "selection", "image", "value" ] P.decodeImageViews
        |> requiredAt [ "data", "selection", "price", "value" ] float
        |> requiredAt [ "data", "selection", "product" ] (list P.decodeLink)
        |> requiredAt [ "data", "selection", "short_lede", "value" ] P.decodeStructuredText


decodeStore : Decoder Store
decodeStore =
    let
        decodeStoreTimes day =
            requiredAt [ "data", "store", day ] (list (field "value" string))
    in
        decode Store
            |> requiredAt [ "id" ] string
            |> requiredAt [ "slugs" ] (list string)
            |> requiredAt [ "tags" ] (list string)
            |> requiredAt [ "data", "store", "address", "value" ] string
            |> requiredAt [ "data", "store", "city", "value" ] string
            |> requiredAt [ "data", "store", "zipcode", "value" ] string
            |> requiredAt [ "data", "store", "country", "value" ] string
            |> requiredAt [ "data", "store", "description", "value" ] P.decodeStructuredText
            |> requiredAt [ "data", "store", "name", "value" ] P.decodeStructuredText
            |> requiredAt [ "data", "store", "image", "value" ] P.decodeImageViews
            |> decodeStoreTimes "monday"
            |> decodeStoreTimes "tuesday"
            |> decodeStoreTimes "wednesday"
            |> decodeStoreTimes "thursday"
            |> decodeStoreTimes "friday"
            |> decodeStoreTimes "saturday"
            |> decodeStoreTimes "sunday"
