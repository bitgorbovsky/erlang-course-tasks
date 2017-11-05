-module(atm_db_api).

-export([update/2,get/1]).

%% Ищет запись по ключу
-spec get( Key :: term()) -> {Value :: term()} | undef.
get(Key) -> 
	case is_process_alive_alias(db) of 
	    true -> 
	        case gen_server:call(db, {find, {Key}}) of
	        	error -> undef;
	        	{ok, Data} -> Data
	    	end;
	    false -> 
	    	undef
    end.

%% Обновляет баланс аккаунта
-spec update(Key :: term(), Value :: term()) -> ok.
update(Key, Value) -> 
	case is_process_alive_alias(db) of 
			true -> 
				gen_server:call(db, {atomic_change_balance, {Key, Value}});
			false -> 
				undef
	end.


is_process_alive_alias(Name) ->
	case whereis(Name) of
		undefined -> false;
		Pid -> is_process_alive(Pid)
	end.