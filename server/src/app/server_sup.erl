-module(server_sup).
-behaviour(supervisor).

-export([start_link/0, 
		 start_child/1, 
		 start_child/2, 
		 init/1]).

start_link() ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).

start_child(Mod) ->
    start_child(Mod, []).

start_child(Mod, Args) ->
    supervisor:start_child(?MODULE,
                         	{Mod, 
						 	{Mod, start_link, Args},
                         	temporary,
						 	10000,
						 	worker,
						 	[Mod]}).

init([]) -> 
	{ok, 
	 {   
		{one_for_one, 3, 10},   
        []
	 }
	}.
