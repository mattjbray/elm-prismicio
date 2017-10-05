module Prismic.Group
    exposing
        ( Group
        , field
        , optional
        , optionalField
        , required
        )

{-|

@docs Group


## Decoders

@docs field, optionalField


## Pipeline decoders

@docs optional, required

-}

import Dict exposing (Dict)
import Prismic.Internal as Internal exposing (..)


{-| A `Group` is a collection of basic `Fields`.

`Groups` cannot contain `Slices` or other `Groups`.

-}
type alias Group =
    Internal.Group


getKey : String -> Dict String v -> Maybe (Result String v)
getKey key =
    Maybe.map Ok << Dict.get key


{-| Decode a field
-}
field : String -> Decoder Field a -> Decoder Group a
field =
    Internal.requiredField getKey


{-| Decode a field that might be missing.
-}
optionalField : String -> Decoder Field a -> a -> Decoder Group a
optionalField =
    Internal.optionalField getKey



-- PIPELINES


{-| -}
required : String -> Decoder Field a -> Decoder Group (a -> b) -> Decoder Group b
required =
    Internal.required getKey


{-| -}
optional : String -> Decoder Field a -> a -> Decoder Group (a -> b) -> Decoder Group b
optional =
    Internal.optional getKey
