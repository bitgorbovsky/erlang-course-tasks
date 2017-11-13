-module(test).

-export([start_pool/1, loop/1]).

start_pool(Port) ->
    {ok, L} = gen_tcp:listen(Port, [{active, true}]),
    listening(L).


loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    listening(ListenSocket),
    tcp_processing_logic(Socket).


listening(ListenSocket) ->
    Pid = spawn(test, loop, [ListenSocket]),
    io:format("listening spawned new loop ~p~n", [Pid]).


tcp_processing_logic(Socket) ->
    receive
        {tcp, Socket, Data} ->
            io:format("~p: received data ~p on socket ~p~n",
                [self(), Data, Socket]
            ),
            tcp_processing_logic(Socket);
        {tcp_closed, Socket} ->
            io:format("~p: connection closed on socket ~p~n",
                [self(), Socket]
            )
    end.

