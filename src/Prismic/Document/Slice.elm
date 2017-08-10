module Prismic.Document.Slice exposing (..)

import Prismic.Document.Field as Field exposing (GroupDecoder, decodeGroup)
import Prismic.Document.Internal as Internal exposing (..)
import Result.Extra as Result


-- TYPES
-- DECODERS


{-| Decodes a `Slice` field.
-}
type Decoder a
    = Decoder (Slice -> Result String a)


oneOf : List (Decoder a) -> Decoder a
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


{-| Decode a (deprecated) old-style slice in a slice zone. The tagger is also passed the slice label.

TODO: custom label decoders?

-}
labelledSlice : String -> (Maybe String -> a -> b) -> Field.Decoder a -> Decoder b
labelledSlice sliceType tagger (Field.Decoder fieldDecoder) =
    Decoder
        (\slice ->
            if sliceType == slice.sliceType then
                case slice.sliceContent of
                    SliceContentField sliceField ->
                        fieldDecoder sliceField
                            |> Result.map (tagger slice.sliceLabel)
                            |> Result.mapError
                                (\msg -> "While decoding slice with type '" ++ slice.sliceType ++ "': " ++ msg)

                    SliceContent _ _ ->
                        Err "Expected an old-style slice but got a new-style one."
            else
                Err ("Expected slice with type '" ++ sliceType ++ "' but got '" ++ slice.sliceType ++ "'.")
        )


{-| Decode a (deprecated) old-style slice in a slice zone.
-}
sliceV1 : String -> (a -> b) -> Field.Decoder a -> Decoder b
sliceV1 sliceType tagger fieldDecoder =
    labelledSlice sliceType (\_ -> tagger) fieldDecoder


{-| Decode a slice in a slice zone.
-}
slice : String -> (a -> List b -> c) -> GroupDecoder a -> GroupDecoder b -> Decoder c
slice sliceType tagger nonRepeatDecoder repeatDecoder =
    Decoder
        (\slice ->
            if sliceType == slice.sliceType then
                case slice.sliceContent of
                    SliceContent doc docs ->
                        Result.map2 tagger
                            (decodeGroup nonRepeatDecoder doc
                                |> Result.mapError
                                    (\msg -> "While decoding non-repeating part: " ++ msg)
                            )
                            (List.map (decodeGroup repeatDecoder) docs
                                |> Result.collect
                                |> Result.mapError
                                    (\msg -> "While decoding repeating part: " ++ msg)
                            )
                            |> Result.mapError
                                (\msg -> "While decoding slice with type '" ++ slice.sliceType ++ "': " ++ msg)

                    SliceContentField _ ->
                        Err "Expected a new-style slice but got an old-style one. Try using sliceV1 instead."
            else
                Err ("Expected slice with type '" ++ sliceType ++ "' but got '" ++ slice.sliceType ++ "'.")
        )


decodeSlice : Decoder a -> Slice -> Result String a
decodeSlice (Decoder f) slice =
    f slice
