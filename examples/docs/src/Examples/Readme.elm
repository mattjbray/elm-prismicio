module Readme exposing (..)

import Prismic exposing (Decoder, Document)
import Prismic.Field as Field exposing (Field)
import Task


-- Create a type corresponding to your Custom Type in Prismic.


type alias MyDocType =
    { content : Field.StructuredText }



-- Describe how to convert a Prismic Document into your custom type.


myDocDecoder : Decoder Document MyDocType
myDocDecoder =
    Prismic.decode MyDocType
        |> Prismic.required "content" Field.structuredText



-- Add the Prismic Model to your Model.


type alias Model =
    { prismic : Prismic.Model
    , response : Maybe (Prismic.Response MyDocType)
    }



-- Initialize Prismic with your API URL. We also start with a request to fetch
-- our home page from Prismic.


init =
    let
        model =
            { prismic =
                Prismic.init "https://lesbonneschoses.prismic.io/api"
            , response =
                Nothing
            }
    in
    ( model, fetchHomePage model.prismic )



{- Querying Prismic -}


type Msg
    = SetHomePage (Result Prismic.PrismicError ( Prismic.Model, Prismic.Response MyDocType ))


fetchHomePage : Prismic.Model -> Cmd Msg
fetchHomePage prismic =
    Prismic.api prismic
        |> Prismic.bookmark "home-page"
        |> Prismic.submit myDocDecoder
        |> Task.attempt SetHomePage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetHomePage (Ok ( prismic, response )) ->
            ( { model
                | prismic =
                    Prismic.cache model.prismic prismic
                , response =
                    Just response
              }
            , Cmd.none
            )

        -- handle the error
        SetHomePage (Err error) ->
            ( model
            , Cmd.none
            )
