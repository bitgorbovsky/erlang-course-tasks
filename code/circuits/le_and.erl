-module(le_and).
-export([
    make/3,
    reset/1
]).

-export([
    init/1,
    handle_message/2,
    terminate/1
]).


make(WireIn1, WireIn2, WireOut) ->
    gen_proc:start(?MODULE, [WireIn1, WireIn2, WireOut]).

reset(And) ->
    gen_proc:send(And, reset).


init([WireIn1, WireIn2, WireOut]) ->
    LE_And = self(),
    SignalSetter = fun(Pin) ->
        fun(Signal) ->
            gen_proc:send(LE_And, {set_pin, Pin, Signal})
        end
    end,
    wire:add_reaction(WireIn1, SignalSetter(0)),
    wire:add_reaction(WireIn2, SignalSetter(1)),
    #{
        out_signal => undefined,
        action => fun(Signal) ->
            wire:signal(WireOut, Signal)
        end
    }.


handle_message(
    {set_pin, _, false},
    #{out_signal := undefined, action := Action} = State
) ->
    Action(false),
    {ok, State#{out_signal => false}};
handle_message({set_pin, Pin, true}, #{out_signal := undefined} = State) ->
    {ok, State#{out_signal => {wait_pin, opposite_pin(Pin)}}};
handle_message(
    {set_pin, Pin, true},
    #{out_signal := {wait_pin, Pin}, action := Action} = State
) ->
    Action(true),
    {ok, State#{out_signal => true}};
handle_message(reset, State) ->
    {ok, State#{signal => undefined}}.

terminate(_) ->
    ok.

opposite_pin(0) -> 1;
opposite_pin(1) -> 0.
