module Json.Decode.Pipeline (required, optional, fromResult, decode, hardcode, delegate) where

{-| # elm-decode-pipeline

A library for building decoders using the pipeline [`(|>)`](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Basics#|>)
operator and ordinary function calls.

## Motivation

It's common to decode into a record that has a `type alias`. Here's an example
of this from the [`object3`](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Json-Decode#object3)
docs:

    type alias Job = { name : String, id : Int, completed : Bool }

    point : Decoder Job
    point =
      object3 Job
        ("name" := string)
        ("id" := int)
        ("completed" := bool)

This works because a record type alias can be called as a normal function. In
that case it accepts one argument for each field (in whatever order the fields
are declared in the type alias) and then returns an appropriate record built
with those arguments.

The `objectN` decoders are straightforward, but require manually changing N
whenever the field count changes. This library provides functions designed to
be used with the `|>` operator, with the goal of having decoders that are both
easy to read and easy to modify.

## Examples

Here is a decoder built with this library.

    import Json.Decode exposing (int, string, float, Decoder)
    import Json.Decode.Pipeline exposing (decode, required, optional, hardcode)


    type alias User =
      { id : Int
      , name : String
      , percentExcited : Float
      }


    userDecoder : Decoder User
    userDecoder =
      decode User
        |> required "id" int
        |> optional "name" string "(fallback if name not present)"
        |> hardcode 1.0

In this example:

* `decode` is a synonym for [`succeed`](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Json-Decode#succeed) (it just reads better here)
* `required "id" int` is similar to `("id" := int)`
* `optional` is like `required`, but if the field is not present, decoding does not fail; instead it succeeds with the provided fallback value.
* `hardcode` does not look at the provided JSON, and instead always decodes to the same value.

You could use this decoder as follows:

    Json.Decode.decodeString
      userDecoder
      """
        {"id": 123, "name": "Sam Sample"}
      """

The result would be:

    { id = 123
    , name = "Sam Sample"
    , percentExcited = 1.0
    }

Alternatively, you could use it like so:

    Json.Decode.decodeString
      userDecoder
      """
        {"id": 123, percentExcited: "hardcoded; will be ignored"}
      """

In this case, the result would be:

    { id = 123
    , name = "(fallback if name not present)"
    , percentExcited = 1.0
    }

## Design Principles

* Introduce functions that work well with `|>`
* Don't introduce any custom infix operators
* Don't introduce any functions that are intended to be called using backticks

@docs required, optional, hardcode, delegate, fromResult, decode
-}

import Json.Decode exposing (Decoder, map, succeed, andThen, (:=), maybe, customDecoder)


{-| -}
required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required key valDecoder decoder =
  andThen decoder (\wrappedFn -> map wrappedFn (key := valDecoder))


{-| -}
optional : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional key valDecoder fallback decoder =
  let
    maybeDecoder =
      andThen
        ((Maybe.withDefault fallback) >> succeed)
        (maybe (key := valDecoder))
  in
    andThen decoder (\wrappedFn -> map wrappedFn maybeDecoder)


{-| -}
hardcode : a -> Decoder (a -> b) -> Decoder b
hardcode val decoder =
  andThen decoder (\wrappedFn -> map wrappedFn (succeed val))


{-| -}
delegate : Decoder a -> Decoder (a -> b) -> Decoder b
delegate delegated decoder =
  andThen decoder (\wrappedFn -> map wrappedFn delegated)


{-| -}
fromResult : Decoder (Result String a) -> Decoder a
fromResult resultDecoder =
  andThen resultDecoder (\result -> customDecoder (succeed ()) (\_ -> result))


{-| -}
decode : a -> Decoder a
decode =
  succeed
