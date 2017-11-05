-module(atm_msg).
-compile(export_all).

hello() ->
    io:format("Please, insert a card.. ~n", []).

pin() ->
    io:format("Please, enter the pin ~n", []).

no_in_db() ->
    io:format("Sorry, but we have not information about your card. Call to support, Tel: 909 ~n", []).

take_card() ->
    io:format("Take the card... ~n", []).

take_money(Amount) ->
    io:format("Take ~p..~n", [Amount]).

print(Msg, Data) ->
    io:format("~p ~p~n", [Msg, Data]).

enter_amount() ->
    io:format("Enter the amount... ~n", []).

attempts_limit() ->
    io:format("Attempts limit ! ~n", []).


wrong_pin() ->
    io:format("Wrong pin, enter again ~n", []).

wrong_amount() ->
    io:format("Wrong amount, enter again ~n", []).

info(Balance) ->
    io:format("Your balace is ~p~n", [Balance]).
menu() ->
    io:format("Enter 'withdraw' to withdraw money or 'add' to add money or 'cancel' to return card ~n", []).

not_impl() ->
    io:format("Not implemented ~n", []).