-module(atm_svisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
        SupervisorSpecification = #{
            strategy => one_for_one,
            intensity => 10,
            period => 60},
    
        ChildSpecifications =
            [#{id => atm,
                start => {atm, start_link, []},
                restart => transient,
                shutdown => 1000,
                type => worker,
                modules => [atm]}
            ],
        {ok, {SupervisorSpecification, ChildSpecifications}}.