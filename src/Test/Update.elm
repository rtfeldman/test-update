module Test.Update exposing (fuzz, fuzzBasic)

{-| Generate all possible models by running randomly-generated sequences of
messages through `update` on an initial model.


## Testing

@docs fuzz, fuzzBasic

-}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test.Runner
import Test exposing (Test)


{-| Given an initial model, an `update`, and a message fuzzer, run a randomly-
generated sequence of messages through `update` on that initial model. Take
the resulting model and run an `Expectation` on it.

This is different from [`fuzzBasic`](#fuzzBasic) in that it accepts an `update` which
returns a `( model, cmd )` tuple rather than a `model` alone.

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        model


    msgFuzzer : Fuzzer Msg
    msgFuzzer =
        Fuzz.oneOf [ NoOp, Bar, Baz ]


    Test.Update.fuzz update msgFuzzer initialModel "username is not empty" <|
        \model ->
            model.username
                |> Expect.notEqual ""

-}
fuzz :
    (msg -> model -> ( model, cmd ))
    -> Fuzzer msg
    -> model
    -> String
    -> (model -> Expectation)
    -> Test
fuzz update =
    fuzzBasic (\msg model -> Tuple.first (update msg model))


{-| Given an initial model, an `update`, and a message fuzzer, run a randomly-
generated sequence of messages through `update` on that initial model. Take
the resulting model and run an `Expectation` on it.

This is different from [`fuzz`](#fuzz) in that it accepts an `update` which
returns a `model` alone rather than a `( model, cmd )` tuple.

    update : Msg -> Model -> Model
    update msg model =
        model


    msgFuzzer : Fuzzer Msg
    msgFuzzer =
        Fuzz.oneOf [ NoOp, Bar, Baz ]


    Test.Update.fuzzBasic update msgFuzzer initialModel "username is not empty" <|
        \model ->
            model.username
                |> Expect.notEqual ""

-}
fuzzBasic :
    (msg -> model -> model)
    -> Fuzzer msg
    -> model
    -> String
    -> (model -> Expectation)
    -> Test
fuzzBasic update msgFuzzer model description verify =
    Test.fuzz (Fuzz.list msgFuzzer) description <|
        test update model verify


test :
    (msg -> model -> model)
    -> model
    -> (model -> Expectation)
    -> List msg
    -> Expectation
test update model verify messages =
    case Test.Runner.getFailure (verify model) of
        Nothing ->
            testUpdateHelp Expect.pass update model verify messages

        Just { given, message } ->
            [ "Initial model failed before any messages were applied."
            , ""
            , "Initial model was:"
            , ""
            , "    " ++ toString model
            , ""
            , "Failure was:"
            , ""
            , message
            ]
                |> String.join "\n"
                |> Expect.fail


testUpdateHelp :
    Expectation
    -> (msg -> model -> model)
    -> model
    -> (model -> Expectation)
    -> List msg
    -> Expectation
testUpdateHelp expectation update model verify messages =
    case messages of
        [] ->
            expectation

        msg :: rest ->
            let
                newModel =
                    update msg model
            in
                case Test.Runner.getFailure (verify newModel) of
                    Nothing ->
                        testUpdateHelp expectation update newModel verify rest

                    Just { given, message } ->
                        [ "Model which passed:"
                        , ""
                        , "    " ++ toString model
                        , ""
                        , "Message applied to that model:"
                        , ""
                        , "    " ++ toString msg
                        , ""
                        , "Resulting model, which failed:"
                        , ""
                        , "    " ++ toString newModel
                        , ""
                        , "Failure:"
                        , ""
                        , message
                        ]
                            |> String.join "\n"
                            |> Expect.fail
