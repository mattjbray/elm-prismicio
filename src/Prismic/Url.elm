module Prismic.Url exposing (Url(Url), withQuery)

{-|

@docs Url, withQuery

-}

import Http


{-| Disambiguate `Url`s from `String`s
-}
type Url
    = Url String


{-| Add query parameters to a `Url` -}
withQuery: List ( String, String ) -> Url -> Url
withQuery  params (Url base) =
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
