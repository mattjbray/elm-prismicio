module Prismic.View exposing (..)

import Dict exposing (Dict)
import Json.Encode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Prismic.Types exposing (..)
import String


asHtmlWithDefault :
    (LinkedDocument -> Url)
    -> Html msg
    -> String
    -> String
    -> Dict String (Dict String (List DocumentField))
    -> Html msg
asHtmlWithDefault linkResolver default documentType fieldName data =
    Maybe.withDefault default
        (Dict.get documentType data
            `Maybe.andThen` Dict.get fieldName
            `Maybe.andThen` (\docs ->
                                Just
                                    (case docs of
                                        [ doc ] ->
                                            asHtml linkResolver doc

                                        _ ->
                                            div [] (List.map (asHtml linkResolver) docs)
                                    )
                            )
        )


asHtml : (LinkedDocument -> Url) -> DocumentField -> Html msg
asHtml linkResolver field =
    case field of
        Text t ->
            span [] [ text t ]

        Date t ->
            span [] [ text t ]

        Number n ->
            span [] [ text (toString n) ]

        Select t ->
            span [] [ text t ]

        Color t ->
            span [] [ text ("<Color> " ++ t) ]

        Link l ->
            linkAsHtml linkResolver l

        Image i ->
            imageAsHtml i.main

        StructuredText fields ->
            div [] (structuredTextAsHtml linkResolver fields)


structuredTextAsHtml : (LinkedDocument -> Url) -> StructuredText -> List (Html msg)
structuredTextAsHtml linkResolver =
    List.map (structuredTextFieldAsHtml linkResolver)


structuredTextFieldAsHtml : (LinkedDocument -> Url) -> StructuredTextField -> Html msg
structuredTextFieldAsHtml linkResolver field =
    case field of
        SSimple simpleField ->
            simpleFieldAsHtml linkResolver simpleField

        SImage image ->
            imageAsHtml image

        SEmbed embed ->
            embedAsHtml embed


simpleFieldAsHtml : (LinkedDocument -> Url) -> SimpleStructuredTextField -> Html msg
simpleFieldAsHtml linkResolver field =
    let
        el =
            case field.fieldType of
                Heading1 ->
                    h1

                Heading2 ->
                    h2

                Heading3 ->
                    h3

                Paragraph ->
                    p

                ListItem ->
                    -- TODO: unify ULs?
                    (\attrs childs ->
                        ul [] [ li attrs childs ]
                    )

        spanEl span =
            case span.spanType of
                Em ->
                    em []

                Strong ->
                    strong []

                Hyperlink link ->
                    linkAsHtmlWith linkResolver link

        foldFn span ( childs, index ) =
            let
                beginning =
                    String.slice index span.start field.text

                middle =
                    String.slice span.start span.end field.text
            in
                ( childs ++ [ text beginning, (spanEl span) [ text middle ] ]
                , span.end
                )
    in
        el []
            (field.spans
                |> List.sortBy .start
                |> List.foldl foldFn ( [], 0 )
                |> (\( childs, index ) -> childs ++ [ text (String.dropLeft index field.text) ])
            )


imageAsHtml : ImageProperties -> Html msg
imageAsHtml image =
    let
        (Url urlStr) =
            image.url
    in
        img [ src urlStr ] []


embedAsHtml : EmbedProperties -> Html msg
embedAsHtml embed =
    case embed of
        EmbedVideo props ->
            div [ property "innerHTML" (Json.string props.html) ] []

        EmbedRich props ->
            div [ property "innerHTML" (Json.string props.html) ] []


linkAsHtml : (LinkedDocument -> Url) -> Link -> Html msg
linkAsHtml linkResolver link =
    case link of
        DocumentLink linkedDoc isBroken ->
            let
                (Url url) =
                    linkResolver linkedDoc
            in
                a [ href url ] [ text (toString linkedDoc.slug) ]

        WebLink (Url url) ->
            a [ href url ] [ text url ]


linkAsHtmlWith : (LinkedDocument -> Url) -> Link -> List (Html msg) -> Html msg
linkAsHtmlWith linkResolver link childs =
    case link of
        DocumentLink linkedDoc isBroken ->
            let
                (Url url) =
                    linkResolver linkedDoc
            in
                a [ href url ] childs

        WebLink (Url url) ->
            a [ href url ] childs


defaultLinkResolver : LinkedDocument -> Url
defaultLinkResolver linkedDoc =
    Url (String.join "/" [ "documents", linkedDoc.id, linkedDoc.slug ])


viewDefaultDocType : DefaultDocType -> Html msg
viewDefaultDocType doc =
    let
        allDocFields =
            let
                fieldsPerType =
                    Dict.values doc

                fieldsPerField =
                    List.concatMap Dict.values fieldsPerType
            in
                List.concat fieldsPerField
    in
        div []
            ([ h2 [] (List.map text (Dict.keys doc)) ]
                ++ List.map (asHtml defaultLinkResolver) allDocFields
            )


getTitle : StructuredText -> Maybe StructuredTextField
getTitle structuredText =
    let
        isTitle field =
            case field of
                SSimple simpleField ->
                    case simpleField.fieldType of
                        Heading1 ->
                            True

                        Heading2 ->
                            True

                        Heading3 ->
                            True

                        _ ->
                            False

                _ ->
                    False
    in
        List.head (List.filter isTitle structuredText)


getFirstParagraph : StructuredText -> Maybe StructuredTextField
getFirstParagraph structuredText =
    let
        isParagraph field =
            case field of
                SSimple simpleField ->
                    case simpleField.fieldType of
                        Paragraph ->
                            True

                        _ ->
                            False

                _ ->
                    False
    in
        List.head (List.filter isParagraph structuredText)


getFirstImage : StructuredText -> Maybe ImageProperties
getFirstImage structuredText =
    let
        getImage field =
            case field of
                SImage image ->
                    Just image

                _ ->
                    Nothing
    in
        List.head (List.filterMap getImage structuredText)


getText : StructuredTextField -> String
getText field =
    case field of
        SSimple simpleField ->
            simpleField.text

        SImage imageField ->
            Maybe.withDefault "<image>" imageField.alt

        _ ->
            ""


getTexts : StructuredText -> String
getTexts fields =
    fields
        |> List.map getText
        |> String.join " "
