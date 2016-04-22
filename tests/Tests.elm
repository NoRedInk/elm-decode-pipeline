module Tests (..) where

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
    ]
