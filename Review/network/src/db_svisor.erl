-module(db_svisor).
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
            [#{id => db,
                start => {db, new, []},
                restart => transient,
                shutdown => 2000,
                type => worker,
                modules => [db]}
            ],
        {ok, {SupervisorSpecification, ChildSpecifications}}.