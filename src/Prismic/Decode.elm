module Prismic.Decode
    exposing
        ( Decoder
        , andThen
        , apply
        , custom
        , decode
        , fail
        , map
        , succeed
        )

{-| Helpers for decoding various parts of a Document.


## Decoder combinators

@docs Decoder, succeed, fail, map, apply, andThen


## Pipeline decoders

@docs decode, custom

-}

import Prismic.Document.Internal as Internal exposing (..)


{-| -}
type alias Decoder val a =
    Internal.Decoder val a


{-| -}
succeed : a -> Decoder val a
succeed =
    Internal.succeed


{-| -}
fail : String -> Decoder val a
fail =
    Internal.fail


{-| Transform a decoder.
-}
map : (a -> b) -> Decoder val a -> Decoder val b
map =
    Internal.map


{-| -}
apply : Decoder val (a -> b) -> Decoder val a -> Decoder val b
apply =
    Internal.apply


{-| -}
andThen : (a -> Decoder val b) -> Decoder val a -> Decoder val b
andThen =
    Internal.andThen



-- Decoding Pipelines


{-| Begin a decoding pipeline.

    type alias MyDoc =
        { title : StructuredText }

    myDocDecoder : Decoder MyDoc
    myDocDecoder =
        decode MyDoc
            |> required "title" structuredText

-}
decode : a -> Decoder val a
decode =
    Internal.decode


{-| -}
custom : Decoder val a -> Decoder val (a -> b) -> Decoder val b
custom =
    Internal.custom
