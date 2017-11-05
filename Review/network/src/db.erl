-module(db).
-behavior(gen_server).


-export([stop/0,new/0,status/0 ]).
-export([init/1, handle_call/3, terminate/2, code_change/3]).


%%% module API

%% Создает новую таблицу (стартует процес)
-spec new() -> ok | {error, Reason :: term()}.
new() ->
    gen_server:start_link({local,db}, ?MODULE, [], []).

%% Удаляет таблицу (останавливает процесс)
-spec stop() -> ok.
stop() ->
    gen_server:call(db, {delete_all_objects}),
    gen_server:stop(db), ok.

-spec status() -> ok.
status() -> ok.

%%% gen_server API

init([]) ->
    State = db_interal:new(),
    State2 = db_interal:append(123,{1, 200},State),
    {ok, State2}.

handle_call({delete, {Key}}, _From, State) ->
    NewState = db_interal:delete(Key,State),
    {reply, ok, NewState};

handle_call({delete_all_objects}, _From, _) ->
    NewState = db_interal:new(),
    {reply, ok, NewState};

handle_call({atomic_change_balance, {Key, Value}}, _From, State) ->
    {Status,Balance, NewState} = db_interal:atomic_change_balance(Key,Value,State),
    {reply, {Status,Balance}, NewState};

handle_call({find, {Key}}, _From, State) ->
    Reply = db_interal:find(Key,State),
    {reply, Reply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.