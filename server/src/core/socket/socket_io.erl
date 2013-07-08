%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-29
%%% Description: TODO: Add description to socket_io
%%% ==================================
-module(socket_io).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/1 ,send/2]).

%%
%% API Functions
%%
start([])->
	{_ ,Port} = core_cfg:get_global_value(tcp_listener_port) ,
	case is_integer(Port) of
		true ->
			init(Port) ;
		_ ->
			error
	end .


%%
%% Local Functions
%%
init(Port)->
	
				%% 开启客户端监控树
   {ok,_} = server_sup:start_child(socket_io_client_sup),
			%% 开启tcp listener监控树
   {ok,_} = server_sup:start_child(socket_io_listener_sup,[Port]) ,
	
	ok.


send(Socket ,  Bin)->
	gen_tcp:send(Socket, Bin) .