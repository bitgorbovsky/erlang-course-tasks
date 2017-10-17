-module(gen_proc).
-export([
    start/2,
    send/2
]).

start(Module, InitArgs) ->
    spawn(fun() ->
        State = Module:init(InitArgs),
        LoopState = loop(Module, State),
        Module:terminate(LoopState)
    end).

send(Proc, Request) ->
    Ref = make_ref(),
    Proc ! {Ref, self(), Request},
    receive
        {Ref, Message} -> Message
    end.

loop(Module, State) ->
    receive
        {Ref, From, Message} ->
            try Module:handle_message(Message, State) of
                {terminate, NewState} ->
                    NewState;
                {reply, Reply, NewState} ->
                    From ! {Ref, Reply},
                    loop(Module, NewState)
            catch
                ErrClass:Reason ->
                    From ! {Ref, {ErrClass, Reason}},
                    loop(Module, State)
            end
    end.

%% Хакер по кличке Lobzik очень любит рефакторить код и он переписал функцию
%% loop() следующим образом, введя функцию recv().

recv() ->
    receive Message -> Message end.

new_send(Proc, Request) ->
    Ref = make_ref(),
    Proc ! {Ref, self(), Request},
    wait_response(Ref).

wait_response(Ref) ->
    case recv() of
        {Ref, Reply} ->
            Reply;
        _ ->
            wait_response(Ref)
    end.

new_loop(Module, State) ->
    {Ref, From, Message} = recv(),
    try Module:handle_message(Message, State) of
        {terminate, NewState} ->
            NewState;
        {reply, Reply, NewState} ->
            From ! {Ref, Reply},
            loop(Module, NewState)
    catch
        ErrClass:Reason ->
            From ! {Ref, {ErrClass, Reason}},
            loop(Module, State)
    end.

%% Но как ни старался бедный Lobzik понять, где тут собака зарыта,
%% код никак не хотел работать. Почему? В чем заключается главная
%% ошибка Lobzik'а?
