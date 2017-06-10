
# elm-prismicio

An Elm SDK for [Prismic.io](https://prismic.io).

For a complete [example application](http://blog.mattjbray.com/elm-prismicio),
check out the `examples/` directory of this repo.

## Example

First, you need to initialise the Prismic `Model`.

```elm
import Prismic as P


-- Create a type corresponding to your Custom Type in Prismic.
type alias MyDocType =
    { content : P.StructuredText }


-- Describe how to convert a Prismic Document into your custom type.
myDocDecoder : P.Decoder MyDocType
myDocDecoder =
    P.decode MyDocType
        |> P.field "content" P.structuredText


-- Add the Prismic Model to your Model.
type alias Model =
    { prismic : P.Model
    , response : Maybe (P.Response MyDocType)
    }


-- Initialize Prismic with your API URL. We also start with a request to fetch
-- our home page from Prismic.
init =
    let
        model =
            { prismic =
                P.init (Url "https://lesbonneschoses.prismic.io/api")
            , response =
                Nothing
            }
    in
        ( model, fetchHomePage model.prismic )
```


### Querying Prismic

To make a Prismic request, you need to do four things:

1. Make sure we have fetched the API details.
2. Select a Form.
3. Optionally customise the Form's query.
4. Submit the Request, providing a decoder to decode the documents in the
   result.

In practice, it will look something like this:

```elm
type Msg
    = SetHomePage (Result P.PrismicError ( P.Response MyDocType, P.Model ))


fetchHomePage prismic =
    P.api prismic
      |> P.form "everything"
      |> P.bookmark "home-page"
      |> P.submit myDocDecoder
      |> Task.attempt SetHomePage
```


When you handle `SetHomePage` in your app's `update` function, you should
combine the `prismic` value in your model with the one returned in the tuple.

```elm
update msg model =
    case msg of
        SetHomePage (Ok ( response, prismic )) ->
            ( { model
                  | prismic =
                      P.collectResponses model.prismic prismic
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

## Custom document types

You should define your own Elm record type for each of your Prismic document
types. You'll also need to define a Decoder for each of your Elm record types,
and pass it to `Prismic.submit` so that the document can be marshalled into your
Elm record type.
