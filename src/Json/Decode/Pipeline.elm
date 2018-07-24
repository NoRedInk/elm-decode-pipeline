module Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt, resolve)

{-|


# Json.Decode.Pipeline

Use the `(|>)` operator to build JSON decoders.


## Decoding fields

@docs required, requiredAt, optional, optionalAt, hardcoded, custom


## Beginning and ending pipelines

@docs decode, resolve

-}

import Json.Decode as Decode exposing (Decoder)


{-| Decode a required field.

    import Json.Decode exposing (Decoder, int, string)
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
    custom (Decode.field key valDecoder) decoder


{-| Decode a required nested field.
-}
requiredAt : List String -> Decoder a -> Decoder (a -> b) -> Decoder b
requiredAt path valDecoder decoder =
    custom (Decode.at path valDecoder) decoder


{-| Decode a field that may be missing or have a null value. If the field is
missing, then it decodes as the `fallback` value. If the field is present,
then `valDecoder` is used to decode its value. If `valDecoder` fails on a
`null` value, then the `fallback` is used as if the field were missing
entirely.

    import Json.Decode exposing (Decoder, int, null, oneOf, string)
    import Json.Decode.Pipeline exposing (decode, optional, required)

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

Because `valDecoder` is given an opportunity to decode `null` values before
resorting to the `fallback`, you can distinguish between missing and `null`
values if you need to:

    userDecoder2 =
        decode User
            |> required "id" int
            |> optional "name" (oneOf [ string, null "NULL" ]) "MISSING"
            |> required "email" string

-}
optional : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional key valDecoder fallback decoder =
    custom (optionalDecoder (Decode.field key Decode.value) valDecoder fallback) decoder


{-| Decode an optional nested field.
-}
optionalAt : List String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optionalAt path valDecoder fallback decoder =
    custom (optionalDecoder (Decode.at path Decode.value) valDecoder fallback) decoder


optionalDecoder : Decoder Decode.Value -> Decoder a -> a -> Decoder a
optionalDecoder pathDecoder valDecoder fallback =
    let
        nullOr decoder =
            Decode.oneOf [ decoder, Decode.null fallback ]

        handleResult input =
            case Decode.decodeValue pathDecoder input of
                Ok rawValue ->
                    -- The field was present, so now let's try to decode that value.
                    -- (If it was present but fails to decode, this should and will fail!)
                    case Decode.decodeValue (nullOr valDecoder) rawValue of
                        Ok finalResult ->
                            Decode.succeed finalResult

                        Err finalErr ->
                            Decode.fail finalErr

                Err _ ->
                    -- Decoding failed. This could be for one of two reasons.
                    -- Either the field was not present, in which case we should
                    -- use the fallback, or we're not working on an object, in
                    -- which case we should fail.
                    --
                    -- https://github.com/NoRedInk/elm-decode-pipeline/issues/40

                    case Decode.decodeValue (Decode.keyValuePairs Decode.value) input of
                        Ok _ ->
                            -- It's an object, but the field was not present,
                            -- so use the fallback.
                            Decode.succeed fallback

                        Err finalErr ->
                            -- It wasn't an object, so fail!
                            Decode.fail finalErr

    in
    Decode.value
        |> Decode.andThen handleResult


{-| Rather than decoding anything, use a fixed value for the next step in the
pipeline. `hardcoded` does not look at the JSON at all.

    import Json.Decode exposing (Decoder, int, string)
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
hardcoded =
    Decode.succeed >> custom


{-| Run the given decoder and feed its result into the pipeline at this point.

Consider this example.

    import Json.Decode exposing (Decoder, at, int, string)
    import Json.Decode.Pipeline exposing (custom, decode, required)

    type alias User =
        { id : Int
        , name : String
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        decode User
            |> required "id" int
            |> custom (at [ "profile", "name" ] string)
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
custom decoder wrapped =
    Decode.map2 (<|) wrapped decoder


{-| Convert a `Decoder (Decoder a)` into a `Decoder a`. Useful when you want
to perform some custom processing just before completing the decoding operation.

    import Json.Decode exposing (Decoder, float, int, string)
    import Json.Decode.Pipeline exposing (decode, required, resolve)

    type alias User =
        { id : Int
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        let
            -- toDecoder gets run *after* all the
            -- (|> required ...) steps are done.
            toDecoder : Int -> String -> Int -> Decoder User
            toDecoder id email version =
                if version > 2 then
                    succeed (User id email)

                else
                    fail "This JSON is from a deprecated source. Please upgrade!"
        in
        decode toDecoder
            |> required "id" int
            |> required "email" string
            |> required "version" int
            -- version is part of toDecoder,
            |> resolve


    -- but it is not a part of User

    result : Result String User
    result =
        Json.Decode.decodeString
            userDecoder
            """
          {"id": 123, "email": "sam@example.com", "version": 1}
        """


    -- Err "This JSON is from a deprecated source. Please upgrade!"

-}
resolve : Decoder (Decoder a) -> Decoder a
resolve =
    Decode.andThen identity


{-| Begin a decoding pipeline. This is a synonym for [Json.Decode.succeed](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode#succeed),
intended to make things read more clearly.

    import Json.Decode exposing (Decoder, float, int, string)
    import Json.Decode.Pipeline exposing (decode, optional, required)

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
    Decode.succeed
