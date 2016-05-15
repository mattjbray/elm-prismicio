module Prismic.Rest exposing (..)

import Http
import Prismic.Types exposing (..)
import Prismic.Decoders exposing (..)
import Task exposing (Task)


fetchApi : Url -> Task Http.Error Api
fetchApi (Url url) =
    Http.get decodeApi url
