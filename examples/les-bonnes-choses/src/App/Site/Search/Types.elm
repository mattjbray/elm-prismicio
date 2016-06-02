module App.Site.Search.Types exposing (..)

import App.Site.Search.Results.Types as Results

type alias Model =
  { query : String
  , page : Page
  , content : Content
  }

type Page
  = IndexP
  | ResultsP String


type Content
  = IndexC
  | ResultsC Results.Model


type Msg
  = ResultsMsg Results.Msg
