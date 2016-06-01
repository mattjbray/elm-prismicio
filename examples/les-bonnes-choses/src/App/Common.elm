module App.Common exposing (..)

import App.Navigation exposing (toHash)
import App.Types as App
import App.Blog.Types as Blog
import App.Site.Types as Site
import Html exposing (Html)
import Prismic.Types as P
import Prismic.View as P


linkResolver : P.LinkedDocument -> P.Url
linkResolver linkedDoc =
    let
        page =
            case linkedDoc.linkedDocumentType of
                "blog-post" ->
                    App.BlogP <| Blog.PostP linkedDoc.id linkedDoc.slug

                _ ->
                    let
                        _ =
                            Debug.log "Cannot resolve linkedDoc: " linkedDoc
                    in
                        App.SiteP <| Site.HomeP
    in
        P.Url (toHash page)


structuredTextAsHtml : P.StructuredText -> List (Html msg)
structuredTextAsHtml =
    P.structuredTextAsHtml linkResolver
