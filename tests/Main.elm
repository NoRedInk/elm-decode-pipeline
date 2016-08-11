port module Main exposing (..)

{-|
Run the tests with node-test-runner:

https://github.com/rtfeldman/node-test-runner
-}

import Tests
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Program Never
main =
    run emit Tests.all


port emit : ( String, Value ) -> Cmd msg
