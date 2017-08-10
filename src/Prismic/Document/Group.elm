module Prismic.Document.Group
    exposing
        ( Decoder
        , field
        , optional
        , optionalField
        , required
        )

{-|

@docs Decoder, field, optional, optionalField, required

-}

import Dict exposing (Dict)
import Prismic.Document.Field as Field
import Prismic.Document.Internal as Internal exposing (..)


{-| -}
type alias Decoder a =
    Internal.Decoder Group a


getKey : String -> Dict String v -> Maybe (Result String v)
getKey key =
    Maybe.map Ok << Dict.get key


{-| Decode a field
-}
field : String -> Field.Decoder a -> Decoder a
field =
    Internal.field getKey


{-| Decode a field that might be missing.
-}
optionalField : String -> Field.Decoder a -> a -> Decoder a
optionalField =
    Internal.optionalField getKey



-- PIPELINES


{-| -}
required : String -> Field.Decoder a -> Decoder (a -> b) -> Decoder b
required =
    Internal.required getKey


{-| -}
optional : String -> Field.Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional =
    Internal.optional getKey
