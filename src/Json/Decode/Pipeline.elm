module Json.Decode.Pipeline (required, optional, resolveResult, decode, hardcode, delegate) where

{-| # Pipeline

Functions for building decoders.

## Design Principles

* Introduce functions that work well with `|>`
* Don't introduce any custom infix operators
* Don't introduce any functions that are intended to be called using backticks

@docs required, optional, hardcode, delegate, resolveResult, decode
-}

import Json.Decode exposing (Decoder, map, succeed, andThen, (:=), maybe, customDecoder)


{-| Decode a required field.

    import Json.Decode exposing (int, string, Decoder)
    import Json.Decode.Pipeline exposing (decode, required)


    type alias User =
      { id : Int
      , name : String
      , email : String
      }


    userDecoder : Decoder User
    userDecoder =
      decode User
        |> required "id" int
        |> required "name" string
        |> required "email" string

We could use this code as follows.

    result : Result String User
    result =
      Json.Decode.decodeString
        userDecoder
        """
          {"id": 123, "email": "sam@example.com", "name": "Sam Sample"}
        """

    -- result == Ok { id = 123, name = "Sam Sample", email = "sam@example.com" }
-}
required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required key valDecoder decoder =
  delegate (key := valDecoder) decoder


{-| Decode a field that may or may not be present. If the field is present,
use the specified decoder on it. If the field is not present, successfully
decode to the given fallback value.

Consider this example.

    import Json.Decode exposing (int, string, Decoder)
    import Json.Decode.Pipeline exposing (decode, required, optional)


    type alias User =
      { id : Int
      , name : String
      , email : String
      }


    userDecoder : Decoder User
    userDecoder =
      decode User
        |> required "id" int
        |> optional "name" string "blah"
        |> required "email" string

We could use this code as follows.

    result : Result String User
    result =
      Json.Decode.decodeString
        userDecoder
        """
          {"id": 123, "email": "sam@example.com" }
        """

    -- result == Ok { id = 123, name = "blah", email = "sam@example.com" }

-}
optional : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional key valDecoder fallback decoder =
  let
    maybeDecoder =
      maybe (key := valDecoder) `andThen` ((Maybe.withDefault fallback) >> succeed)
  in
    delegate maybeDecoder decoder


{-| Rather than decoding anything, use a fixed value for the next step in the
pipeline. `harcode` does not look at the JSON at all.

    import Json.Decode exposing (int, string, Decoder)
    import Json.Decode.Pipeline exposing (decode, required)


    type alias User =
      { id : Int
      , email : String
      , loginAttempts : Int
      }


    userDecoder : Decoder User
    userDecoder =
      decode User
        |> required "id" int
        |> required "email" string
        |> hardcode 0

We could use this code as follows.

    result : Result String User
    result =
      Json.Decode.decodeString
        userDecoder
        """
          {"id": 123, "email": "sam@example.com"}
        """

    -- result == Ok { id = 123, email = "sam@example.com", loginAttempts = 0 }
-}
hardcode : a -> Decoder (a -> b) -> Decoder b
hardcode val decoder =
  andThen decoder (\wrappedFn -> map wrappedFn (succeed val))


{-| Delegate the next step in the pipeline to the given decoder. (If that
decoder succeeds, its result gets fed into the pipeline at this point.)

Consider this example.

    import Json.Decode exposing (int, string, at, Decoder)
    import Json.Decode.Pipeline exposing (decode, required, optional, delegate)


    type alias User =
      { id : Int
      , name : String
      , email : String
      }


    userDecoder : Decoder User
    userDecoder =
      decode User
        |> required "id" int
        |> delegate (at [ "profile", "name" ])
        |> required "email" string

We could use this code as follows.

    result : Result String User
    result =
      Json.Decode.decodeString
        userDecoder
        """
          {"id": 123, "email": "sam@example.com", {"profile": {"name": "Sam"}}
        """

    -- result == Ok { id = 123, name = "Sam", email = "sam@example.com" }
-}
delegate : Decoder a -> Decoder (a -> b) -> Decoder b
delegate delegated decoder =
  andThen decoder (\wrappedFn -> map wrappedFn delegated)


{-| Convert a `Decoder (Result x a)` into a `Decoder a`. Useful when you want
to perform some custom processing just before completing the decoding operation.

`resolveResult` is typically used as follows.

    import Json.Decode exposing (int, string, float, Decoder)
    import Json.Decode.Pipeline exposing (decode, required, optional, hardcode)


    type alias User =
      { id : Int
      , email : String
      }


    userDecoder : Decoder User
    userDecoder =
      let
        -- asResult gets run *after* all the (|> required ...) steps are done.
        asResult Int -> String -> Int -> Result String User
        asResult id email version =
          if version > 2 then
            Ok (User id email)
          else
            Err "This JSON is from a deprecated endpoint. Please upgrade!"
      in
        decode asResult
          |> required "id" int
          |> required "email" string
          |> required "version" int -- version is part of asResult, but not User
          |> resolveResult

We could use this code as follows.

    result : Result String User
    result =
      Json.Decode.decodeString
        userDecoder
        """
          {"id": 123, "email": "sam@example.com", "version": 1}
        """

    -- result == Err "This JSON is from a deprecated endpoint. Please upgrade!"
-}
resolveResult : Decoder (Result String a) -> Decoder a
resolveResult resultDecoder =
  andThen resultDecoder (\result -> customDecoder (succeed ()) (\_ -> result))


{-| Begin a decoding pipeline. This is a synonym for [Json.Decode.succeed](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Json-Decode#succeed),
intended to make things read more clearly.

    import Json.Decode exposing (int, string, float, Decoder)
    import Json.Decode.Pipeline exposing (decode, required, optional)


    type alias User =
      { id : Int
      , email : String
      , name : String
      }


    userDecoder : Decoder User
    userDecoder =
      decode User
        |> required "id" int
        |> required "email" string
        |> optional "name" string ""
-}
decode : a -> Decoder a
decode =
  succeed
