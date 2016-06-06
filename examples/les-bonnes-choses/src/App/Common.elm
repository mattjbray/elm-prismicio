module App.Common exposing (..)

import App.Navigation exposing (toHash, linkResolver, urlForBlog, urlForSearch)
import App.Site.Jobs.Types as Jobs
import App.Site.Stores.Types as Stores
import App.Site.Types as Site
import App.Types as App
import Html exposing (..)
import Html.Attributes exposing (classList, id, href, style)
import Prismic as P


structuredTextAsHtml : P.StructuredText -> List (Html msg)
structuredTextAsHtml =
    P.structuredTextAsHtml linkResolver


toCssUrl : P.Url -> String
toCssUrl (P.Url url) =
    "url(" ++ url ++ ")"


viewLoading : Maybe P.Url -> Html msg
viewLoading backgroundImageUrl =
    section [ id "page-header" ]
        [ div
            (backgroundImageUrl
                |> Maybe.map (\url -> [ style [ ( "background-image", toCssUrl url ) ] ])
                |> Maybe.withDefault []
            )
            [ div []
                [ h1 [] [ text "Loading..." ]
                ]
            ]
        ]


viewError : P.PrismicError -> Html msg
viewError error =
    pre [] [ text (toString error) ]


viewHeader : App.Page -> Html msg
viewHeader currentPage =
    let
        mkHeaderLink page linkText =
            a
                [ href (toHash (App.SiteP page))
                , classList [ ( "selected", currentPage == App.SiteP page ) ]
                ]
                [ text linkText ]
    in
        header []
            [ nav []
                [ h1 []
                    [ mkHeaderLink Site.HomeP "Les bonnes choses" ]
                , ul []
                    [ li [] [ mkHeaderLink Site.AboutP "About" ]
                    , li [] [ mkHeaderLink (Site.StoresP Stores.IndexP) "Stores" ]
                    ]
                , ul []
                    [ li [] [ mkHeaderLink (Site.JobsP Jobs.IndexP) "Jobs" ]
                    , li []
                        [ a [ href urlForBlog ]
                            [ text "Blog" ]
                        ]
                    ]
                , a [ href urlForSearch ]
                    [ span [] [ text "Search" ] ]
                ]
            ]
