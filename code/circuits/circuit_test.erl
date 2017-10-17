-module(circuit_test).
-compile(export_all).

test_1() ->
    W1 = wire:make(),
    W2 = wire:make(),
    W3 = wire:make(),

    le_and:make(W1, W2, W3),

    wire:add_reaction(W3, fun(S) ->
        io:format("Result: ~p~n", [S])
    end),

    wire:signal(W1, true),
    wire:signal(W2, true).

test_2() ->
    W1 = wire:make(),
    W2 = wire:make(),
    W3 = wire:make(),

    le_and:make(W1, W2, W3),

    wire:add_reaction(W3, fun(S) ->
        io:format("Result: ~p~n", [S])
    end),

    wire:signal(W1, false),
    wire:signal(W2, true).
