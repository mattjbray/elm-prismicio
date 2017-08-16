module Prismic.Slice
    exposing
        ( FieldDecoder
        , Slice
        , field
        , group
        , labelledV1Slice
        , oneOf
        , slice
        , v1Slice
        )

{-|

@docs Slice

@docs oneOf, slice


## Deprecated Slices

@docs FieldDecoder, field, group, labelledV1Slice, v1Slice

-}

import Prismic.Internal as Internal exposing (..)
import Result.Extra as Result


-- TYPES


{-| -}
type alias Slice =
    Internal.Slice



-- DECODERS


{-| -}
oneOf : List (Decoder Slice a) -> Decoder Slice a
oneOf sliceDecoders =
    Decoder
        (\slice ->
            let
                go decoders errors =
                    case decoders of
                        [] ->
                            Err
                                ("No slices matched: \n* "
                                    ++ String.join "\n* " errors
                                )

                        (Decoder decoder) :: rest ->
                            case decoder slice of
                                Ok x ->
                                    Ok x

                                Err err ->
                                    go rest (err :: errors)
            in
            go sliceDecoders []
        )



-- V1 SLICES (deprecated)


{-| -}
type alias FieldDecoder a =
    Internal.Decoder SliceContentV1 a


{-| Decode a (deprecated) old-style slice in a slice zone. The tagger is also passed the slice label.

TODO: custom label decoders?

-}
labelledV1Slice : String -> (Maybe String -> a -> b) -> FieldDecoder a -> Decoder Slice b
labelledV1Slice sliceType tagger fieldDecoder =
    Decoder
        (\slice ->
            if sliceType == slice.sliceType then
                case slice.sliceContent of
                    SliceContentV1 sliceField ->
                        decodeValue fieldDecoder sliceField
                            |> Result.map (tagger slice.sliceLabel)
                            |> Result.mapError
                                (\msg -> "While decoding slice with type '" ++ slice.sliceType ++ "': " ++ msg)

                    SliceContentV2 _ _ ->
                        Err "Expected an old-style slice but got a new-style one."
            else
                Err ("Expected slice with type '" ++ sliceType ++ "' but got '" ++ slice.sliceType ++ "'.")
        )


{-| Decode a (deprecated) old-style slice in a slice zone.
-}
v1Slice : String -> (a -> b) -> FieldDecoder a -> Decoder Slice b
v1Slice sliceType tagger fieldDecoder =
    labelledV1Slice sliceType (\_ -> tagger) fieldDecoder


{-| -}
field : Decoder Field a -> FieldDecoder a
field fieldDecoder =
    Decoder
        (\sliceContent ->
            case sliceContent of
                SliceContentV1Field field ->
                    decodeValue fieldDecoder field

                SliceContentV1Groups _ ->
                    Err "Expected a Field but got a Group. (Hint: use group to decode Groups.)"
        )


{-| -}
group : Decoder Group a -> FieldDecoder (List a)
group groupDecoder =
    Decoder
        (\sliceContent ->
            case sliceContent of
                SliceContentV1Field field ->
                    Err "Expected a Field but got a Group. (Hint: use group to decode Groups.)"

                SliceContentV1Groups groups ->
                    groups
                        |> List.map (decodeValue groupDecoder)
                        |> Result.collect
        )



-- V2 Slices


{-| Decode a slice in a slice zone.
-}
slice : String -> (a -> List b -> c) -> Decoder Group a -> Decoder Group b -> Decoder Slice c
slice sliceType tagger nonRepeatDecoder repeatDecoder =
    Decoder
        (\slice ->
            if sliceType == slice.sliceType then
                case slice.sliceContent of
                    SliceContentV2 doc docs ->
                        Result.map2 tagger
                            (decodeValue nonRepeatDecoder doc
                                |> Result.mapError
                                    (\msg -> "While decoding non-repeating part: " ++ msg)
                            )
                            (List.map (decodeValue repeatDecoder) docs
                                |> Result.collect
                                |> Result.mapError
                                    (\msg -> "While decoding repeating part: " ++ msg)
                            )
                            |> Result.mapError
                                (\msg -> "While decoding slice with type '" ++ slice.sliceType ++ "': " ++ msg)

                    SliceContentV1 _ ->
                        Err "Expected a new-style slice but got an old-style one. Try using v1Slice instead."
            else
                Err ("Expected slice with type '" ++ sliceType ++ "' but got '" ++ slice.sliceType ++ "'.")
        )
