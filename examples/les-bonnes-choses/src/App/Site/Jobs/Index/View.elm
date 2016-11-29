module App.Site.Jobs.Index.View exposing (..)

import App.Navigation exposing (urlForJob)
import App.Site.Jobs.Index.Types exposing (..)
import App.Site.Article.View as Article
import App.Documents.Types as Documents
import Html exposing (..)
import Html.Attributes exposing (..)
import Prismic as P exposing (Url(Url))
import Result.Extra as Result


view : Model -> Html Msg
view model =
    div [ class "main", id "jobs" ]
        [ Html.map ArticleMsg (Article.view model.article)
        , viewJobs model
        ]


viewError : P.PrismicError -> List (Html msg)
viewError error =
    [ pre [] [ text (toString error) ] ]


viewJobs : Model -> Html msg
viewJobs model =
    section
        [ id "page-body"
        , style [ ( "margin-top", "-120px" ) ]
        ]
        (model.jobs
            |> Result.unpack viewError
                (\jobs ->
                    List.concatMap (viewJobsByService jobs)
                        [ ( Just "Store", "Positions in our Stores" )
                        , ( Just "Office", "Positions in our Offices" )
                        , ( Just "Workshop", "Positions in our Workshops" )
                        , ( Nothing, "Other positions" )
                        ]
                )
        )


viewJobsByService :
    List Documents.JobOffer
    -> ( Maybe String, String )
    -> List (Html msg)
viewJobsByService jobs ( service, title ) =
    [ h2 [] [ text title ]
    , div [ class "listing" ]
        (jobs
            |> List.filter (\job -> job.service == service)
            |> List.map viewJob
        )
    ]


viewJob : Documents.JobOffer -> Html msg
viewJob job =
    div [ class "job" ]
        [ a [ href (urlForJob job) ]
            [ h3 [] [ text (P.getTexts job.name) ]
            , p []
                [ text
                    (P.getFirstParagraph job.profile
                        |> Maybe.map P.getText
                        |> Maybe.withDefault "No job profile"
                    )
                ]
            , strong [] [ text "Learn more" ]
            ]
        ]
