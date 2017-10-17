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
        state => init,
        wait_pin => undefined,
        action => fun(Signal) ->
            wire:signal(WireOut, Signal)
        end
    }.


handle_message({set_pin, Pin, Level}, State) ->
    NewState = set_pin(Pin, Level, State),
    {reply, ok, NewState};
handle_message(reset, State) ->
    {reply, ok, State#{state => init, wait_pin => undefined}}.

terminate(_) ->
    ok.

set_pin(_, false, #{state := init, action := Action} = State) ->
    Action(false),
    State#{state => resolved};
set_pin(_, false, #{state := wait, action := Action} = State) ->
    Action(false),
    State#{state => resolved};
set_pin(Pin, true, #{state := init} = State) ->
    State#{state => wait, wait_pin => opposite_pin(Pin)};
set_pin(Pin, true, #{state := wait, wait_pin := Pin, action := Action} = State) ->
    Action(true),
    State#{state := resolved};
set_pin(_, _, #{state := resolved} = State) ->
    State.

opposite_pin(0) -> 1;
opposite_pin(1) -> 0.
