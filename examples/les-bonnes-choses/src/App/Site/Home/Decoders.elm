module App.Site.Home.Decoders exposing (..)

import App.Documents.Decoders as Documents
import App.Site.Home.Types exposing (..)
import Json.Decode exposing (..)


decodeFeatured : Decoder Featured
decodeFeatured =
  oneOf
    [ object1 BlogPost Documents.decodeBlogPost
    , object1 Product Documents.decodeProduct
    ]
