-module(echo).
-behaviour(gen_server).

-export([
    start_link/0,
    echo/2
]).

-export([
    init/1,
    code_change/3,
    terminate/2
]).

-export([
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

echo(Server, String) ->
    gen_server:call(Server, {echo, String}).


init([]) ->
    Port = open_port(
        {spawn, "python echo.py"},
        [
            use_stdio,
            stream,
            binary
        ]
    ),
    {ok, Port}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    port_close(State),
    ok.


handle_call({echo, Str}, _From, State) ->
    port_command(State, Str),
    Result = receive
        {State, {data, Data}} ->
            Data
    end,
    {reply, Result, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Message: ~p", [Msg]),
    {noreply, State}.
