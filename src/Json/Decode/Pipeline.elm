module Json.Decode.Pipeline (required, optional, fromResult, decode, hardcode, delegate) where

{-| # elm-decode-pipeline

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
      maybe (key := valDecoder) `andThen` ((Maybe.withDefault fallback) >> succeed)
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
