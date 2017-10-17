-module(wire).
-export([
    make/0,
    signal/1,
    signal/2,
    add_reaction/2,
    reset/1
]).

-export([
    init/1,
    handle_message/2,
    terminate/1
]).

make() ->
    gen_proc:start(?MODULE, []).

signal(Wire) ->
    gen_proc:send(Wire, signal).

signal(Wire, Signal) ->
    gen_proc:send(Wire, {signal, Signal}).

add_reaction(Wire, Action) ->
    gen_proc:send(Wire, {add_reaction, Action}).

reset(Wire) ->
    gen_proc:send(Wire, reset).

init(_) ->
    #{
        signal => undefined,
        reaction => []
    }.

handle_message(signal, #{signal := Signal} = State) ->
    {reply, Signal, State};
handle_message({signal, Signal}, #{signal := Signal} = State) ->
    {reply, ok, State};
handle_message({signal, Signal}, #{reaction := Actions} = State) ->
    [Action(Signal) || Action <- Actions],
    {reply, ok, State#{signal => Signal}};
handle_message({add_reaction, Action}, #{reaction := Actions} = State) ->
    {reply, ok, State#{reaction => [Action|Actions]}};
handle_message(reset, State) ->
    {reply, ok, State#{signal => undefined}}.


terminate(_) ->
    ok.
