
# elm-prismicio

An Elm SDK for [Prismic.io](https://prismic.io).

For a complete example application, check out the `examples/` directory of this
repo.

## Set up

First, you need to initialise Prismic's Model.

    type alias Model =
        { prismic : Prismic.Model }

    init =
        let
            model =
                { prismic =
                    Prismic.init (Url "https://lesbonneschoses.prismic.io/api")
                }
        in
            ( model, fetchHomePage model.prismic )


## Querying Prismic

To make a Prismic request, you need to do four things:

1. Make sure we have fetched the API.
2. Select a Form.
3. Optionally customise the Form's query.
4. Submit the Request, providing a JSON decoder to decode the documents in the
   result.

In practice, it will look something like this:

    type Msg
        = SetPrismicError P.PrismicError
        | SetHomePage ( P.DefaultDocType, P.Model )

    fetchHomePage prismic =
        P.fetchApi prismic
          |> P.form "everything"
          |> P.bookmark "home-page"
          |> P.submit P.decodeDefaultDocType
          |> Task.perform SetPrismicError SetHomePage


When you handle `SetHomePage` in your app's `update` function, you should
replace the `prismic` value in your model with the one returned in the tuple.

## Custom document types

## View helpers
