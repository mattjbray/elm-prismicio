module App.Site.Search.State exposing (..)

import App.Navigation exposing (toHash)
import App.Types as App exposing (GlobalMsg)
import App.Site.Types as Site
import App.Site.Search.Types exposing (..)
import App.Site.Search.Results.State as Results
import Navigation
import Prismic.Types as P


init : P.Cache -> Page -> ( Model, Cmd Msg )
init prismic page =
    let
        model =
            { page = page
            , content = IndexC
            , query = ""
            }
    in
        case page of
            IndexP ->
                ( model, Cmd.none )

            ResultsP query ->
                let
                    ( results, resultsCmd ) =
                        Results.init prismic query
                in
                    ( { model
                        | content = ResultsC results
                        , query = query
                      }
                    , Cmd.map ResultsMsg resultsCmd
                    )


update : Msg -> Model -> ( Model, Cmd Msg, List GlobalMsg )
update msg model =
    case msg of
        SetQuery query ->
            ( { model
                | query = query
              }
            , Cmd.none
            , []
            )

        Submit ->
            ( model
            , Navigation.newUrl <| toHash <| App.SiteP <| Site.SearchP <| ResultsP model.query
            , []
            )

        ResultsMsg resultsMsg ->
            case model.content of
                ResultsC results ->
                    let
                        ( newResults, resultsCmd, globalMsgs ) =
                            Results.update resultsMsg results
                    in
                        ( { model | content = ResultsC newResults }
                        , Cmd.map ResultsMsg resultsCmd
                        , globalMsgs
                        )

                IndexC ->
                    ( model, Cmd.none, [] )
