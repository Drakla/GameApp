%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-29
%%% Description: TODO: Add description to socket_io_client_sup
%%% ==================================
-module(socket_io_client_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
	{ok,
	 {
	  %% 重启次数
	  {simple_one_for_one, 3, 10},
	  [
	   {
		connect_reader, {connect_reader,start_link,[]},
		temporary, 
		brutal_kill, 
		worker,
		[connect_reader]
	   }
	  ]
	 }
	}.

