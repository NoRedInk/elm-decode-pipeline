module Json.Decode.Pipeline exposing (required, requiredAt, optional, optionalAt, resolveResult, decode, hardcoded, custom)

{-| ## Design Principles

* Introduce functions that work well with the [`(|>)`](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Basics#|>) operator
* Don't introduce any custom infix operators
* Don't introduce any functions that are intended to be called using backticks

@docs required, requiredAt, optional, optionalAt, hardcoded, custom, resolveResult, decode
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


    result : Result String User
    result =
      Json.Decode.decodeString
        userDecoder
        """
          {"id": 123, "email": "sam@example.com", "name": "Sam"}
        """
    -- Ok { id = 123, name = "Sam", email = "sam@example.com" }
-}
required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required key valDecoder decoder =
  custom (key := valDecoder) decoder


{-| Decode a required nested field.
-}
requiredAt : List String -> Decoder a -> Decoder (a -> b) -> Decoder b
requiredAt path valDecoder decoder =
  custom (Json.Decode.at path valDecoder) decoder


{-| Decode a field that may or may not be present. If the field is present,
use the specified decoder on it. If the field is not present, successfully
decode to the given fallback value.

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


    result : Result String User
    result =
      Json.Decode.decodeString
        userDecoder
        """
          {"id": 123, "email": "sam@example.com" }
        """
    -- Ok { id = 123, name = "blah", email = "sam@example.com" }

-}
optional : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional key valDecoder fallback decoder =
  custom (optionalDecoder (key := Json.Decode.value) valDecoder fallback) decoder


{-| Decode an optional nested field.
-}
optionalAt : List String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optionalAt path valDecoder fallback decoder =
  custom (optionalDecoder (Json.Decode.at path Json.Decode.value) valDecoder fallback) decoder


optionalDecoder : Decoder Json.Decode.Value -> Decoder a -> a -> Decoder a
optionalDecoder pathDecoder valDecoder fallback =
  let
    handleResult input =
      case Json.Decode.decodeValue pathDecoder input of
        Ok rawValue ->
          -- The field was present, so now let's try to decode that value.
          -- (If it was present but fails to decode, this should and will fail!)
          Json.Decode.decodeValue valDecoder rawValue

        Err _ ->
          -- The field was not present, so use the fallback.
          Ok fallback
  in
    Json.Decode.customDecoder Json.Decode.value handleResult


{-| Rather than decoding anything, use a fixed value for the next step in the
pipeline. `harcoded` does not look at the JSON at all.

    import Json.Decode exposing (int, string, Decoder)
    import Json.Decode.Pipeline exposing (decode, required)


    type alias User =
      { id : Int
      , email : String
      , followers : Int
      }


    userDecoder : Decoder User
    userDecoder =
      decode User
        |> required "id" int
        |> required "email" string
        |> hardcoded 0


    result : Result String User
    result =
      Json.Decode.decodeString
        userDecoder
        """
          {"id": 123, "email": "sam@example.com"}
        """
    -- Ok { id = 123, email = "sam@example.com", followers = 0 }
-}
hardcoded : a -> Decoder (a -> b) -> Decoder b
hardcoded val decoder =
  andThen decoder (\wrappedFn -> map wrappedFn (succeed val))


{-| Run the given decoder and feed its result into the pipeline at this point.

Consider this example.

    import Json.Decode exposing (int, string, at, Decoder)
    import Json.Decode.Pipeline exposing (decode, required, custom)


    type alias User =
      { id : Int
      , name : String
      , email : String
      }


    userDecoder : Decoder User
    userDecoder =
      decode User
        |> required "id" int
        |> custom (at [ "profile", "name" ])
        |> required "email" string


    result : Result String User
    result =
      Json.Decode.decodeString
        userDecoder
        """
          {
            "id": 123,
            "email": "sam@example.com",
            "profile": {"name": "Sam"}
          }
        """
    -- Ok { id = 123, name = "Sam", email = "sam@example.com" }
-}
custom : Decoder a -> Decoder (a -> b) -> Decoder b
custom delegated decoder =
  andThen decoder (\wrappedFn -> map wrappedFn delegated)


{-| Convert a `Decoder (Result x a)` into a `Decoder a`. Useful when you want
to perform some custom processing just before completing the decoding operation.

    import Json.Decode exposing (int, string, float, Decoder)
    import Json.Decode.Pipeline exposing
      (decode, required, resolveResult)


    type alias User =
      { id : Int
      , email : String
      }


    userDecoder : Decoder User
    userDecoder =
      let
        -- asResult gets run *after* all the
        -- (|> required ...) steps are done.
        asResult Int -> String -> Int -> Result String User
        asResult id email version =
          if version > 2 then
            Ok (User id email)
          else
            Err "This JSON is from a deprecated source. Please upgrade!"
      in
        decode asResult
          |> required "id" int
          |> required "email" string
          |> required "version" int -- version is part of asResult,
          |> resolveResult          -- but it is not a part of User


    result : Result String User
    result =
      Json.Decode.decodeString
        userDecoder
        """
          {"id": 123, "email": "sam@example.com", "version": 1}
        """
    -- Err "This JSON is from a deprecated source. Please upgrade!"
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
