
# elm-prismicio

An Elm SDK for [Prismic.io](https://prismic.io).

For a complete [example application](https://mattjbray.github.io/elm-prismicio),
check out the `examples/` directory of this repo.

## Usage

First, you need to create your types and initialise the Prismic `Model`.


```elm
import Prismic exposing (Decoder, Document)
import Prismic.Field as Field exposing (Field)
import Task


-- Create a type corresponding to your Custom Type in Prismic.


type alias MyDocType =
    { content : Field.StructuredText }



-- Describe how to convert a Prismic Document into your custom type.


myDocDecoder : Decoder Document MyDocType
myDocDecoder =
    Prismic.decode MyDocType
        |> Prismic.required "content" Field.structuredText



-- Add the Prismic Model to your Model.


type alias Model =
    { prismic : Prismic.Model
    , response : Maybe (Prismic.Response MyDocType)
    }



-- Initialize Prismic with your API URL. We also start with a request to fetch
-- our home page from Prismic.


init : ( { prismic : Prismic.Model, response : Maybe a }, Cmd Msg )
init =
    let
        model =
            { prismic =
                Prismic.init "https://lesbonneschoses.prismic.io/api"
            , response =
                Nothing
            }
    in
    ( model, fetchHomePage model.prismic )
```


### Querying Prismic

To make a Prismic request, you need to do four things:

1. Make sure we have fetched the API metadata.
2. Select a Form (a kind of default query in Prismic).
3. Optionally customise the Form's query.
4. Submit the Request, providing a decoder to marshal your documents from the
   result.

In practice, it will look something like this:

```elm
type Msg
    = SetHomePage (Result Prismic.PrismicError ( Prismic.Model, Prismic.Response MyDocType ))


fetchHomePage : Prismic.Model -> Cmd Msg
fetchHomePage prismic =
    Prismic.api prismic
        |> Prismic.bookmark "home-page"
        |> Prismic.submit myDocDecoder
        |> Task.attempt SetHomePage
```


When you handle `SetHomePage` in your app's `update` function, you should
combine the `prismic` value in your model with the one returned in the tuple.

This adds the API metadata and document results to the cache in the Prismic
model, so we don't have to fetch them again next time.

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetHomePage (Ok ( prismic, response )) ->
            ( { model
                | prismic =
                    Prismic.cache model.prismic prismic
                , response =
                    Just response
              }
            , Cmd.none
            )

        -- handle the error
        SetHomePage (Err error) ->
            ( model
            , Cmd.none
            )
```

If you have nested components that use Prismic, you'll need to thread the
Prismic `Model` through your `init` and `update` functions. See the use of the
`GlobalMsg` type in the `examples/` directory for one way of doing this.


## Example

The example appliation in `examples/website` implements the [sample website from
Prismic.io](https://user-guides.prismic.io/examples/nodejs-samples/sample-multi-page-site-with-navigation-in-nodejs).

To run it:

```
make examples
open examples/website/index.html
```
