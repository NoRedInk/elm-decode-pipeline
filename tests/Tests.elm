module Tests exposing (..)

import Expect exposing (Expectation)
import Json.Decode exposing (Decoder, null, string)
import Json.Decode.Pipeline
    exposing
        ( decode
        , optional
        , optionalAt
        , required
        , requiredAt
        , resolve
        )
import Test exposing (..)


{-| Run some JSON through a Decoder and return the result.
-}
runWith : String -> Decoder a -> Result String a
runWith =
    flip Json.Decode.decodeString


isError : Result err ok -> Bool
isError result =
    case result of
        Err _ ->
            True

        Ok _ ->
            False


expectErr : Result err ok -> Expectation
expectErr result =
    isError result
        |> Expect.true ("Expected an Err but got " ++ toString result)


all : Test
all =
    describe
        "Json.Decode.Pipeline"
        [ test "should decode basic example" <|
            \() ->
                decode (,)
                    |> required "a" string
                    |> required "b" string
                    |> runWith """{"a":"foo","b":"bar"}"""
                    |> Expect.equal (Ok ( "foo", "bar" ))
        , test "should decode requiredAt fields" <|
            \() ->
                decode (,)
                    |> requiredAt [ "a" ] string
                    |> requiredAt [ "b", "c" ] string
                    |> runWith """{"a":"foo","b":{"c":"bar"}}"""
                    |> Expect.equal (Ok ( "foo", "bar" ))
        , test "should decode optionalAt fields" <|
            \() ->
                decode (,)
                    |> optionalAt [ "a", "b" ] string "--"
                    |> optionalAt [ "x", "y" ] string "--"
                    |> runWith """{"a":{},"x":{"y":"bar"}}"""
                    |> Expect.equal (Ok ( "--", "bar" ))
        , test "optional succeeds if the field is not present" <|
            \() ->
                decode (,)
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> runWith """{"x":"five"}"""
                    |> Expect.equal (Ok ( "--", "five" ))
        , test "optional succeeds with fallback if the field is present but null" <|
            \() ->
                decode (,)
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> runWith """{"a":null,"x":"five"}"""
                    |> Expect.equal (Ok ( "--", "five" ))
        , test "optional succeeds with result of the given decoder if the field is null and the decoder decodes nulls" <|
            \() ->
                decode (,)
                    |> optional "a" (null "null") "--"
                    |> optional "x" string "--"
                    |> runWith """{"a":null,"x":"five"}"""
                    |> Expect.equal (Ok ( "null", "five" ))
        , test "optional fails if it's not decoding an object" <|
            \() ->
                decode (,)
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> runWith """[]"""
                    |> expectErr
        , test "optional fails if the field is present but doesn't decode" <|
            \() ->
                decode (,)
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> runWith """{"x":5}"""
                    |> expectErr
        , test "optionalAt fails if the field is present but doesn't decode" <|
            \() ->
                decode (,)
                    |> optionalAt [ "a", "b" ] string "--"
                    |> optionalAt [ "x", "y" ] string "--"
                    |> runWith """{"a":{},"x":{"y":5}}"""
                    |> expectErr
        , test "resolve bubbles up decoded Err results" <|
            \() ->
                decode Json.Decode.fail
                    |> required "error" string
                    |> resolve
                    |> runWith """{"error":"invalid"}"""
                    |> expectErr
        , test "resolve bubbles up decoded Ok results" <|
            \() ->
                decode Json.Decode.succeed
                    |> required "ok" string
                    |> resolve
                    |> runWith """{"ok":"valid"}"""
                    |> Expect.equal (Ok "valid")
        ]
