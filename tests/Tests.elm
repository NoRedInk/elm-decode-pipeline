module Tests exposing (..)

import ElmTest exposing (..)
import String
import Json.Decode.Pipeline as Pipeline
import Json.Decode as Json


decode : String -> Json.Decoder a -> Result String a
decode =
  flip Json.decodeString


all : Test
all =
  suite
    "Json.Decode.Pipeline"
    [ Pipeline.decode (,)
        |> Pipeline.required "a" Json.string
        |> Pipeline.required "b" Json.string
        |> decode """{"a":"foo","b":"bar"}"""
        |> assertEqual (Ok ( "foo", "bar" ))
        |> test "should decode basic example"
    , Pipeline.decode (,)
        |> Pipeline.requiredAt [ "a" ] Json.string
        |> Pipeline.requiredAt [ "b", "c" ] Json.string
        |> decode """{"a":"foo","b":{"c":"bar"}}"""
        |> assertEqual (Ok ( "foo", "bar" ))
        |> test "should decode requiredAt fields"
    , Pipeline.decode (,)
        |> Pipeline.optionalAt [ "a", "b" ] Json.string "--"
        |> Pipeline.optionalAt [ "x", "y" ] Json.string "--"
        |> decode """{"a":{},"x":{"y":"bar"}}"""
        |> assertEqual (Ok ( "--", "bar" ))
        |> test "should decode optionalAt fields"
    , Pipeline.decode (,)
        |> Pipeline.optional "a" Json.string "--"
        |> Pipeline.optional "x" Json.string "--"
        |> decode """{"x":"five"}"""
        |> assertEqual (Ok ( "--", "five" ))
        |> test "optional succeeds if the field is not present"
    , Pipeline.decode (,)
        |> Pipeline.optional "a" Json.string "--"
        |> Pipeline.optional "x" Json.string "--"
        |> decode """{"x":5}"""
        |> assertEqual (Err "Expecting something custom but instead got: {\"x\":5}")
        |> test "optional fails if the field is present but doesn't decode"
    , Pipeline.decode (,)
        |> Pipeline.optionalAt [ "a", "b" ] Json.string "--"
        |> Pipeline.optionalAt [ "x", "y" ] Json.string "--"
        |> decode """{"a":{},"x":{"y":5}}"""
        |> assertEqual (Err "Expecting something custom but instead got: {\"a\":{},\"x\":{\"y\":5}}")
        |> test "optionalAt fails if the field is present but doesn't decode"
    ]
