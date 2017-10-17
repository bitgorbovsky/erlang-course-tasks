-module(gen_proc).
-export([
    start/2,
    send/2
]).

start(Module, InitArgs) ->
    Pid = spawn(fun() ->
        State = Module:init(InitArgs),
        loop(Module, State),
        Module:terminate(State)
    end),
    send(Pid, ping),
    Pid.

send(Proc, Request) ->
    Ref = erlang:make_ref(),
    Proc ! {Ref, self(), Request},
    receive
        {Ref, Message} ->
            Message
    end.

loop(Module, State) ->
    receive
        {Ref, From, ping} ->
            From ! {Ref, pong},
            loop(Module, State);
        {Ref, From, Message} ->
            {Reply, NewState} = try
                Module:handle_message(Message, State)
            catch
                ErrClass:Reason ->
                    {{error, ErrClass, Reason}, State}
            end,
            From ! {Ref, Reply},
            loop(Module, NewState)
    end.
