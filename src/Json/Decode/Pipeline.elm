module Json.Decode.Pipeline (required, optional, fromResult, decode, hardcode, delegate) where

{-| # Pipeline

Functions for building decoders.

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
