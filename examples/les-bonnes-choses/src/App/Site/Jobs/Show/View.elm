module App.Site.Jobs.Show.View exposing (..)

import App.Site.Jobs.Show.Types exposing (..)
import App.Documents.Types as Documents
import App.Common exposing (structuredTextAsHtml, viewLoading, toCssUrl)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, selected, style, src)
import Result.Extra as Result
import Prismic as P


view : Model -> Html Msg
view model =
    div
        [ class "main"
        , id "job"
        ]
        (model.job
            |> Result.unpack viewError
                (viewMJob (getBackgroundImageUrl model))
        )


getBackgroundImageUrl : Model -> P.Url
getBackgroundImageUrl model =
    model.article.article
        |> Result.withDefault Nothing
        |> Maybe.map (\article -> article.image.main.url)
        |> Maybe.withDefault (P.Url "")


viewError : P.PrismicError -> List (Html msg)
viewError error =
    [ pre [] [ text (toString error) ] ]


viewMJob : P.Url -> Maybe Documents.JobOffer -> List (Html msg)
viewMJob backgroundImageUrl mJob =
    mJob
        |> Maybe.map (viewJob backgroundImageUrl)
        |> Maybe.withDefault [ viewLoading (Just backgroundImageUrl) ]


viewJob : P.Url -> Documents.JobOffer -> List (Html msg)
viewJob backgroundImageUrl job =
    [ section [ id "page-header" ]
        [ div [ style [ ( "background-image", toCssUrl backgroundImageUrl ) ] ]
            [ div []
                [ h1 [] [ text (P.getTexts job.name) ] ]
            ]
        ]
    , section [ id "page-body" ]
        ([ h2 [] [ text "About you" ] ]
            ++ structuredTextAsHtml job.profile
            ++ [ h2 [] [ text "Your responsibilities" ] ]
            ++ structuredTextAsHtml job.jobDescription
        )
    ]
