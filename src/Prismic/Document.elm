module Prismic.Document exposing (..)

{-|

@docs Document


## Decoding documents

@docs Decoder, succeed, fail, map, apply, andThen
@docs field, optionalField
@docs decode, required, optional, custom
@docs sliceZone
@docs decodeDocument

-}

import Dict exposing (Dict)
import Prismic.Document.Field as Field
import Prismic.Document.Internal as Internal exposing (..)
import Prismic.Document.Slice as Slice
import Result.Extra as Result


{-| Holds the Prismic document.

You will decode this into your own document type by passing a `Decoder MyDoc` to
`submit`.

-}
type alias Document =
    Internal.Document



--  DOCUMENT DECODERS


{-| A value that knows how to decode Documents.

Construct a `Decoder` to pass to `submit`.

-}
type Decoder a
    = Decoder (Document -> Result String a)


{-| -}
succeed : a -> Decoder a
succeed x =
    Decoder (\_ -> Ok x)


{-| -}
fail : String -> Decoder a
fail msg =
    Decoder (\_ -> Err msg)


{-| Internal: Run a `Decoder` against a `Document`.
-}
decodeDocument : Decoder a -> Document -> Result String a
decodeDocument (Decoder decoder) doc =
    decoder doc


{-| Transform a decoder.
-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decoder) =
    Decoder
        (\doc ->
            decoder doc |> Result.map f
        )


{-| -}
apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply (Decoder f) (Decoder a) =
    Decoder
        (\doc ->
            case ( f doc, a doc ) of
                ( Ok g, Ok x ) ->
                    Ok (g x)

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err
        )


{-| -}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f (Decoder a) =
    Decoder
        (\doc ->
            case a doc of
                Ok x ->
                    let
                        (Decoder g) =
                            f x
                    in
                    g doc

                Err err ->
                    Err err
        )


{-| Decode a field
-}
field : String -> Field.Decoder a -> Decoder a
field key fieldDecoder =
    optionalField key (fieldDecoder |> Field.map Just) Nothing
        |> andThen
            (\res ->
                case res of
                    Just x ->
                        succeed x

                    Nothing ->
                        fail ("No field at " ++ key)
            )


{-| Decode a field that might be missing.
-}
optionalField : String -> Field.Decoder a -> a -> Decoder a
optionalField key fieldDecoder default =
    Decoder
        (\(Document doc) ->
            case Dict.get key doc of
                Just (Field field) ->
                    Field.decodeField fieldDecoder field
                        |> Result.mapError
                            (\msg ->
                                "While decoding field '" ++ key ++ "': " ++ msg
                            )

                Just (SliceZone _) ->
                    [ "Expected a Field but got a SliceZone."
                    , "(Hint: use sliceZone to decode this.)"
                    ]
                        |> String.join " "
                        |> Err

                Nothing ->
                    Ok default
        )



-- Decoding Pipelines


{-| Begin a decoding pipeline.

    type alias MyDoc =
        { title : StructuredText }

    myDocDecoder : Decoder MyDoc
    myDocDecoder =
        decode MyDoc
            |> required "title" structuredText

-}
decode : a -> Decoder a
decode =
    succeed


{-| -}
custom : Decoder a -> Decoder (a -> b) -> Decoder b
custom a f =
    apply f a


{-| Decode a required field.
-}
required : String -> Field.Decoder a -> Decoder (a -> b) -> Decoder b
required key fieldDecoder decoder =
    custom (field key fieldDecoder) decoder


{-| Decode a field that might be missing.
-}
optional : String -> Field.Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional key fieldDecoder default decoder =
    custom (optionalField key fieldDecoder default) decoder


{-| Decode a SliceZone.

Pass this function a list of possible elements that can appear in the Slice.

    type alias MyDoc =
        { section : List Section }

    type Section
        = MyContent StructuredText
        | MyImageGallery (List ImageViews)
        | MyLinksSection LinksSection

    type alias LinksSection =
        { title : StructuredText
        , links : List Link
        }

    myDocDecoder : Decode MyDoc
    myDocDecoder =
        decode MyDoc
            |> sliceZone "section"
                (Slice.oneOf
                    [ -- The "my-content" slice type has a non-repeatable zone, but
                      -- no repeatable zone.
                      slice "my-content"
                        (\content () -> MyContent content)
                        structuredText
                        (decode ())
                    , -- The "my-image-gallery" slice type has a repeatable
                      -- zone, but no non-repeatable zone.
                      slice "my-image-gallery"
                        (\() images -> MyImageGallery images)
                        (decode ())
                        image
                    , -- The "my-links-section" slice type has both repeatable
                      -- and non-repeatable zones.
                      slice "my-links-section"
                        (\title links -> MyLinksSection (LinksSection title links))
                        (field "title" structuredText)
                        (field "link" link)
                    ]
                )

-}
sliceZone : String -> Slice.Decoder a -> Decoder (List a -> b) -> Decoder b
sliceZone key sliceDecoder rest =
    apply rest <|
        Decoder
            (\(Document doc) ->
                case Dict.get key doc of
                    Just (SliceZone slices) ->
                        slices
                            |> List.map (Slice.decodeSlice sliceDecoder)
                            |> Result.collect

                    _ ->
                        Err "Expected a SliceZone field."
            )
