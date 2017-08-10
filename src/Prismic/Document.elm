module Prismic.Document
    exposing
        ( Decoder
        , Document
        , field
        , group
        , optional
        , optionalField
        , required
        , sliceZone
        )

{-|

@docs Document


## Decoding documents

@docs Decoder
@docs field, optionalField
@docs group, sliceZone


## Pipeline decoders

@docs required, optional

-}

import Dict exposing (Dict)
import Prismic.Document.Field as Field
import Prismic.Document.Group as Group
import Prismic.Document.Internal as Internal exposing (..)
import Prismic.Document.Slice as Slice
import Result.Extra as Result


{-| Holds the Prismic document.

`Documents` consist of basic `Fields`, `Groups` and `Slices`.

You will decode this into your own document type by passing a `Decoder MyDoc` to
`submit`.

-}
type alias Document =
    Internal.Document



--  DOCUMENT DECODERS


{-| A value that knows how to decode Documents.

Construct a `Decoder` to pass to `submit`.

-}
type alias Decoder a =
    Internal.Decoder Document a


getKey : String -> Document -> Maybe (Result String Field)
getKey key (Document doc) =
    case Dict.get key doc of
        Just (Field field) ->
            Just (Ok field)

        Just (SliceZone _) ->
            [ "Expected a Field but got a SliceZone."
            , "(Hint: use sliceZone to decode this.)"
            ]
                |> String.join " "
                |> Err
                |> Just

        Just (Groups _) ->
            [ "Expected a Field but got a Group."
            , "(Hint: use group to decode this.)"
            ]
                |> String.join " "
                |> Err
                |> Just

        Nothing ->
            Nothing


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


{-| Decode a required field.
-}
required : String -> Field.Decoder a -> Decoder (a -> b) -> Decoder b
required =
    Internal.required getKey


{-| Decode a field that might be missing.
-}
optional : String -> Field.Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional =
    Internal.optional getKey


{-| Decode a SliceZone.

TODO: Update this example.

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
sliceZone : String -> Slice.Decoder a -> Decoder (List a)
sliceZone key sliceDecoder =
    Decoder
        (\(Document doc) ->
            case Dict.get key doc of
                Just (SliceZone slices) ->
                    slices
                        |> List.map (decodeValue sliceDecoder)
                        |> Result.collect

                _ ->
                    Err "Expected a SliceZone field."
        )


{-| Decode a group.

TODO: Update this example.

Groups are essentially Documents, so you pass `group` a Document `Decoder`.

Here is an example with a slice containing groups:

    type alias MyDoc =
        { slices : List Slice }

    type Slice
        = SAlbum Album
        | SBook Book

    type alias Album =
        { title : String
        , cover : ImageViews
        }

    type alias Book =
        { title : String
        , blurb : StructuredText
        }

    albumDecoder : Decoder Album
    albumDecoder =
        decode Album
            |> required "title" text
            |> required "cover" image

    bookDecoder : Decoder Book
    bookDecoder =
        decode Book
            |> required "title" text
            |> required "blurb" structuredText

    myDocDecoder : Decoder MyDoc
    myDocDecoder =
        decode MyDoc
            |> required "slices"
                (sliceZone
                    [ slice "album" (group albumDecoder)
                    , slice "book" (group bookDecoder)
                    ]
                )

-}
group : String -> Group.Decoder a -> Decoder (List a)
group key decoder =
    Decoder
        (\(Document doc) ->
            case Dict.get key doc of
                Just (Groups groups) ->
                    groups
                        |> List.map (decodeValue decoder)
                        |> Result.collect

                Just field ->
                    Err ("Expected a Group field, but got '" ++ toString field ++ "'.")

                Nothing ->
                    Ok []
        )
