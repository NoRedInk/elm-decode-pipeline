module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Json.Decode.Pipeline as Pipeline
import Json.Decode as Json


decode : String -> Json.Decoder a -> Result String a
decode =
    flip Json.decodeString


all : Test
all =
    describe
        "Json.Decode.Pipeline"
        [ Pipeline.decode (,)
            |> Pipeline.required "a" Json.string
            |> Pipeline.required "b" Json.string
            |> decode """{"a":"foo","b":"bar"}"""
            |> Expect.equal (Ok ( "foo", "bar" ))
            |> always
            |> test "should decode basic example"
        , Pipeline.decode (,)
            |> Pipeline.requiredAt [ "a" ] Json.string
            |> Pipeline.requiredAt [ "b", "c" ] Json.string
            |> decode """{"a":"foo","b":{"c":"bar"}}"""
            |> Expect.equal (Ok ( "foo", "bar" ))
            |> always
            |> test "should decode requiredAt fields"
        , Pipeline.decode (,)
            |> Pipeline.optionalAt [ "a", "b" ] Json.string "--"
            |> Pipeline.optionalAt [ "x", "y" ] Json.string "--"
            |> decode """{"a":{},"x":{"y":"bar"}}"""
            |> Expect.equal (Ok ( "--", "bar" ))
            |> always
            |> test "should decode optionalAt fields"
        , Pipeline.decode (,)
            |> Pipeline.optional "a" Json.string "--"
            |> Pipeline.optional "x" Json.string "--"
            |> decode """{"x":"five"}"""
            |> Expect.equal (Ok ( "--", "five" ))
            |> always
            |> test "optional succeeds if the field is not present"
        , Pipeline.decode (,)
            |> Pipeline.optional "a" Json.string "--"
            |> Pipeline.optional "x" Json.string "--"
            |> decode """{"a":null,"x":"five"}"""
            |> Expect.equal (Ok ( "--", "five" ))
            |> always
            |> test "optional succeeds if the field is present but null"
        , Pipeline.decode (,)
            |> Pipeline.optional "a" Json.string "--"
            |> Pipeline.optional "x" Json.string "--"
            |> decode """{"x":5}"""
            |> Expect.equal (Err "A `customDecode` failed with the message: Expecting a String but instead got: 5")
            |> always
            |> test "optional fails if the field is present but doesn't decode"
        , Pipeline.decode (,)
            |> Pipeline.optionalAt [ "a", "b" ] Json.string "--"
            |> Pipeline.optionalAt [ "x", "y" ] Json.string "--"
            |> decode """{"a":{},"x":{"y":5}}"""
            |> Expect.equal (Err "A `customDecode` failed with the message: Expecting a String but instead got: 5")
            |> always
            |> test "optionalAt fails if the field is present but doesn't decode"
        , Pipeline.decode Err
            |> Pipeline.required "error" Json.string
            |> Pipeline.resolveResult
            |> decode """{"error":"invalid"}"""
            |> Expect.equal (Err "A `customDecode` failed with the message: invalid")
            |> always
            |> test "resolveResult bubbles up decoded Err results"
        , Pipeline.decode Ok
            |> Pipeline.required "ok" Json.string
            |> Pipeline.resolveResult
            |> decode """{"ok":"valid"}"""
            |> Expect.equal (Ok "valid")
            |> always
            |> test "resolveResult bubbles up decoded Ok results"
        ]
