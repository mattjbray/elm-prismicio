module Prismic.Url
    exposing
        ( Url(Url)
        , decodeUrl
        , withQuery
        )

{-|

@docs Url, withQuery, decodeUrl

-}

import Http
import Json.Decode as Json


{-| Disambiguate `Url`s from `String`s
-}
type Url
    = Url String


{-| JSON Decoder for `Urls`.
-}
decodeUrl : Json.Decoder Url
decodeUrl =
    Json.map Url Json.string


{-| Add query parameters to a `Url`
-}
withQuery : List ( String, String ) -> Url -> Url
withQuery params (Url base) =
    let
        sep =
            if List.isEmpty params then
                ""
            else
                "?"

        joinParamPair ( key, val ) =
            Http.encodeUri key ++ "=" ++ Http.encodeUri val

        paramsPart =
            params
                |> List.map joinParamPair
                |> String.join "&"
    in
    Url (base ++ sep ++ paramsPart)
