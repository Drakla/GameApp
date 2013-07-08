%%% ==================================
%%% Author: hanyq
%%% Created: 2013-6-6
%%% Description: TODO: Add description to socket_io_tcp_acceptor_sup
%%% ==================================
-module(socket_io_acceptor_sup).
-behaviour(supervisor).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_link/0 ,init/1]).

%%
%% API Functions
%%
start_link()->
	
	  supervisor:start_link({local,?MODULE}, ?MODULE, []).


%%
%% Local Functions
%%
init([]) ->
	%%返回创建子进程的参数，创建sd_tcp_acceptor类的子进程
	{ok, {
		  {simple_one_for_one, 3, 10},
		  [
		   	{socket_io_acceptor, {socket_io_acceptor, start_link, []},
				transient, brutal_kill, worker, [socket_io_acceptor]}]
		 }
	}.