-module(main_svisor).
-behaviour(supervisor).

-export([start_link/0, start/2]).
-export([init/1]).

start(StartType, StartArgs) ->
        start_link().

start_link() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
        SupervisorSpecification = #{
            strategy => one_for_all,
            intensity => 2,
            period => 60000},
    
        ChildSpecifications =
            [#{id => atm_svisor,
                start => {atm_svisor, start_link, []},
                restart => transient,
                shutdown => 3000,
                type => supervisor,
                modules => [atm_svisor]}
            ],
            [#{id => db_svisor,
                start => {db_svisor, start_link, []},
                restart => transient,
                shutdown => 3000,
                type => supervisor,
                modules => [db_svisor]}
            ],
        {ok, {SupervisorSpecification, ChildSpecifications}}.