-module(atm).
-behaviour(gen_statem).
-define(NAME, atm).

-export([start_link/0]).
-export([button/1]).
-export([init/1, callback_mode/0,terminate/3,code_change/4]).
-export([waiting_for_pin/3, waiting_for_card/3, insert_card/1,menu/3, waiting_for_withdraw_amount/3,waiting_for_add_amount/3]).

start_link() ->
    gen_statem:start_link({local,?NAME}, ?MODULE, [], []).

-spec insert_card(CardNo :: '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')->       
insert_card(CardNo) ->
    gen_statem:call(?NAME, {cardNo, CardNo}).

-spec button(Button :: enter|withdraw|add|cancel|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')->            
button(Digit) ->
    gen_statem:call(?NAME, {button,Digit}).

init([]) ->
    Data = #{balance => [], remainingInput => [], pin => [], cardNum => [], tryCount => 1},
    {ok, waiting_for_card, Data}.

waiting_for_card(enter, _, Data) ->
    atm_msg:hello(),
    {keep_state, Data#{remainingInput := []}};


waiting_for_card({call, From}, {cardNo, CardNumber}, Data) ->
    case atm_db_api:get(to_int(CardNumber)) of
        undef -> 
            atm_msg:no_in_db(),
            returning_card(Data, From);
        {Pin, Balance} -> 
            {next_state, waiting_for_pin, Data#{pin := Pin, balance := Balance, cardNum := to_int(CardNumber)}, [{state_timeout, get_env(no_pin_timeout), return}, {reply,From,waiting_for_pin}]}
    end.

waiting_for_pin(state_timeout, return, #{remainingInput := Remaining} = Data) ->
    case Remaining of
        [] -> 
            returning_card(Data);           
        _ -> 
            {next_state, waiting_for_pin, Data}
    end;

waiting_for_pin(enter, _, Data) ->
    atm_msg:pin(),
    {keep_state, Data};

waiting_for_pin({call,From}, _,  #{tryCount := TryCount} = Data) when TryCount > 3  ->
    atm_msg:attempts_limit(),
    returning_card(Data, From);  

waiting_for_pin({call,From}, {button, Digit}, 
    #{pin := Pin, remainingInput := RemainingPin, tryCount := TryCount} = Data) -> 
    case Digit of
        enter -> 
           case check_pin(Pin, to_int(RemainingPin)) of
                true -> 
                    {next_state, menu, Data, [{reply,From,menu}]};
                false -> 
                    atm_msg:wrong_pin(),
                    {keep_state, Data#{remainingInput := [], tryCount:= TryCount+1}, [{reply,From,waiting_for_pin}]}
            end;
        cancel -> 
            case RemainingPin of
                [] -> 
                    returning_card(Data, From);                    
                _ -> 
                    atm_msg:pin(), 
                    {keep_state, Data#{remainingInput := []}, [{reply,From,waiting_for_pin}]}
            end;
        _ -> 
            atm_msg:print("Entered : ", to_int(RemainingPin ++ Digit)),
            {keep_state, Data#{remainingInput := RemainingPin ++ Digit}, [{reply,From,waiting_for_pin}]}
    end.

menu(enter, _, #{balance := Balance} = Data) ->
    atm_msg:menu(),
    atm_msg:info(Balance),
    {keep_state, Data#{remainingInput := []}};

menu({call,From}, {button, Digit}, Data) -> 
case Digit of
    withdraw -> 
        {next_state, waiting_for_withdraw_amount, Data, [{reply,From,waiting_for_withdraw_amount}]};
    add ->
        atm_msg:not_impl(),
        {next_state, waiting_for_add_amount, Data, [{reply,From,waiting_for_add_amount}]};
    cancel -> 
        returning_card(Data, From)
end.


waiting_for_withdraw_amount(enter, _, Data) ->
    atm_msg:enter_amount(),
    {keep_state, Data};

waiting_for_withdraw_amount({call,From}, {button,Digit},
#{balance := Balance, remainingInput := EnteredAmont, cardNum := CardNumber} = Data) -> 
    case Digit of
        enter -> 
            case atm_db_api:update(CardNumber, -to_int(EnteredAmont)) of
                    {success,NewBalance} -> 
                        atm_msg:info(NewBalance), 
                        atm_msg:take_money(EnteredAmont),
                        returning_card(Data, From);
                    {wrong_amount,_} -> 
                        atm_msg:wrong_amount(),
                        {keep_state, Data#{remainingInput := []}, [{reply,From,waiting_for_withdraw_amount}]};
                    undef -> 
                        atm_msg:no_in_db(),
                        returning_card(Data, From)
            end;
        cancel -> 
            case EnteredAmont of
                    [] -> 
                        returning_card(Data, From);
                    _ -> 
                        atm_msg:enter_amount(), 
                        {keep_state, Data#{remainingInput := []}, [{reply,From,waiting_for_withdraw_amount}]}
                end;
        _ -> 
            {keep_state, Data#{remainingInput := EnteredAmont ++ Digit}, [{reply,From,waiting_for_withdraw_amount}]}
    end.

waiting_for_add_amount(enter, _, Data) ->
    atm_msg:enter_amount(),
    {keep_state, Data};

waiting_for_add_amount({call,From}, {button,Digit},
#{balance := Balance, remainingInput := EnteredAmont, cardNum := CardNumber} = Data) -> 
    case Digit of
        enter -> 
            case atm_db_api:update(CardNumber, to_int(EnteredAmont)) of
                {success,NewBalance} ->             
                    atm_msg:info(NewBalance), 
                    returning_card(Data, From);
                {wrong_amount,_} -> 
                    atm_msg:wrong_amount(),
                    {keep_state, Data#{remainingInput := []}, [{reply,From,waiting_for_add_amount}]};
                undef -> 
                    atm_msg:no_in_db(),
                    returning_card(Data, From)
            end;
        cancel -> 
            case EnteredAmont of
                    [] -> 
                        returning_card(Data, From);
                    _ -> 
                        atm_msg:enter_amount(), 
                        {keep_state, Data#{remainingInput := []}, [{reply,From,waiting_for_add_amount}]}
                end;
        _ -> 
            {keep_state, Data#{remainingInput := EnteredAmont ++ Digit}, [{reply,From,waiting_for_add_amount}]}
    end.

check_pin(Pin, Input) ->
    Pin == Input.

to_int(String) -> 
    {Int, _} = string:to_integer(String), Int.

returning_card(Data, From) ->
    atm_msg:take_card(),
    {next_state, waiting_for_card, Data#{remainingInput := [], tryCount:= 0}, [{reply,From,waiting_for_card}]}.

returning_card(Data) ->
    atm_msg:take_card(),
    {next_state, waiting_for_card, Data#{remainingInput := [], tryCount:= 0}}.

terminate(_Reason, State, _Data) ->
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

get_env(Key) ->
        {ok, Val} = application:get_env(atm, Key), Val.

callback_mode() ->
    [state_functions, state_enter].