-module(circuit_test).
-include_lib("eunit/include/eunit.hrl").

le_and_true_true_test() ->
    W1 = wire:make(),
    W2 = wire:make(),
    W3 = wire:make(),
    le_and:make(W1, W2, W3),

    Self = self(),
    wire:add_reaction(W3, fun(S) ->
        Self ! wire_3_done
    end),

    wire:signal(W1, true),
    wire:signal(W2, true),

    receive wire_3_done -> ok end,

    ?assert(wire:signal(W3)).

le_and_false_true_test() ->
    W1 = wire:make(),
    W2 = wire:make(),
    W3 = wire:make(),
    le_and:make(W1, W2, W3),

    Self = self(),
    wire:add_reaction(W3, fun(S) ->
        Self ! wire_3_done
    end),

    wire:signal(W1, false),
    wire:signal(W2, true),

    receive wire_3_done -> ok end,

    ?assert(not wire:signal(W3)).

le_and_true_false_test() ->
    W1 = wire:make(),
    W2 = wire:make(),
    W3 = wire:make(),
    le_and:make(W1, W2, W3),

    Self = self(),
    wire:add_reaction(W3, fun(S) ->
        Self ! wire_3_done
    end),

    wire:signal(W1, true),
    wire:signal(W2, false),

    receive wire_3_done -> ok end,

    ?assert(not wire:signal(W3)).


le_and_false_false_test() ->
    W1 = wire:make(),
    W2 = wire:make(),
    W3 = wire:make(),
    le_and:make(W1, W2, W3),

    Self = self(),
    wire:add_reaction(W3, fun(S) ->
        Self ! wire_3_done
    end),

    wire:signal(W1, false),
    wire:signal(W2, false),

    receive wire_3_done -> ok end,

    ?assert(not wire:signal(W3)).
