module Test.Update exposing (testUpdate, fuzzUpdate)

{-|


## Testing

@docs testUpdate, fuzzUpdate

-}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test.Runner
import Test exposing (Test)


{-| -}
fuzzUpdate :
    Fuzzer msg
    -> String
    -> (msg -> model -> model)
    -> model
    -> (model -> Expectation)
    -> Test
fuzzUpdate msgFuzzer description update model postcondition =
    Test.fuzz (Fuzz.list msgFuzzer) description <|
        testUpdate update model postcondition


{-| -}
testUpdate :
    (msg -> model -> model)
    -> model
    -> (model -> Expectation)
    -> List msg
    -> Expectation
testUpdate =
    testUpdateHelp Expect.pass


testUpdateHelp :
    Expectation
    -> (msg -> model -> model)
    -> model
    -> (model -> Expectation)
    -> List msg
    -> Expectation
testUpdateHelp expectation update model postcondition messages =
    case messages of
        [] ->
            expectation

        msg :: rest ->
            let
                newModel =
                    update msg model
            in
                case Test.Runner.getFailure (postcondition newModel) of
                    Nothing ->
                        testUpdateHelp expectation update newModel postcondition rest

                    Just { given, message } ->
                        [ "Previous model:"
                        , ""
                        , "    " ++ toString model
                        , ""
                        , "Message applied to that model:"
                        , ""
                        , "    " ++ toString msg
                        , ""
                        , "Resulting model:"
                        , ""
                        , "    " ++ toString newModel
                        , ""
                        , "Failure:"
                        , ""
                        , message
                        ]
                            |> String.join "\n"
                            |> Expect.fail
