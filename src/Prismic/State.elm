module Prismic.State exposing (..)

import Dict
import Prismic.Types exposing (..)


initCache : Url -> Cache
initCache url =
  { api = Nothing
  , url = url
  , nextRequestId = 0
  , requests = Dict.empty
  , responses = Dict.empty
  }
