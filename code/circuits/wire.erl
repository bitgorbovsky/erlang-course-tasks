-module(wire).
-export([
    make/0,
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


handle_message({signal, Signal}, #{signal := Signal} = State) ->
    {ok, State};
handle_message({signal, Signal}, #{reaction := Actions} = State) ->
    [Action(Signal) || Action <- Actions],
    {ok, State#{signal => Signal}};
handle_message({add_reaction, Action}, #{reaction := Actions} = State) ->
    {ok, State#{reaction => [Action|Actions]}};
handle_message(reset, State) ->
    {ok, State#{signal => undefined}}.


terminate(_) ->
    ok.
