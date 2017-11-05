-module(db_interal).

-export([
new/0,
append/3,
update/3,
atomic_change_balance/3,
delete/2,
find/2
]).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


new() -> 
    maps:new().

append(Key, Element, Db) -> 
    Db#{Key => Element}.

update(Key, Element, Db) -> 
    Db#{Key := Element}.

atomic_change_balance(Key, Amount, Db) -> 
    {ok, {Pin, _Balance}} =  maps:find(Key, Db), 
    NewBalance =  _Balance + Amount,
    case NewBalance >= 0 of
        true -> 
            {success, NewBalance, Db#{Key := {Pin, NewBalance}}};
        false -> 
            {wrong_amount, _Balance, Db}
    end.

delete(Key, Db) -> 
    maps:remove(Key, Db).

find(Key, Db) -> 
    maps:find(Key, Db).

-ifdef(TEST).
new_test_ () ->
    [
    ? _assert (new () =:=  #{})
    ].
append_test_ () ->
    [
    ? _assert (append (francesco,'london', #{}) =:=  #{francesco => london}),
    ? _assert (append (lelle, 'Stockholm', #{francesco => london}) 
    =:=  #{francesco => london, lelle => 'Stockholm'})
    ].
update_test_ () ->
    [
    ? _assert (update (francesco, 'Stockholm', #{francesco => london}) 
    =:=  #{francesco =>'Stockholm'})
    ].
batch_delete_test_ () ->
    [
    ? _assert (delete(lelle, #{francesco => london,lelle => 'Stockholm'}) 
    =:=  #{francesco => london})
    ].
batch_read_test_ () ->
    [
    ? _assert (find(lelle, #{francesco => london,lelle => 'Stockholm'}) 
        =:=  {ok,'Stockholm'}),
    ? _assert (find(1, #{francesco => london,lelle => 'Stockholm'}) 
    =:=  error)
    ].
atomic_change_balance_test_ () ->
        [
        ? _assert (atomic_change_balance(123,12,#{123 => {1,200}}) 
        =:=  {success,212,#{123 => {1,212}}})
        ].
-endif.

