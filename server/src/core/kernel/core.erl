%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-28
%%% Description: TODO: Add description to core
%%% ==================================
-module(core).

%%
%% Include files
%%
-include("common.hrl").
%%
%% Exported Functions
%%
-export([start/1]).

%%
%% API Functions
%%
start([])->
	
	{ ok,	_} = server_sup:start_child(timer_ticker),	
 	{ ok,	_} = server_sup:start_child(core_srv),	
 	{ ok,	_} = server_sup:start_child(core_rand_srv) ,
 	{ ok,	_} = server_sup:start_child(core_register_srv),
 	%%{ ok,	_} = server_sup:start_child(cron),
	
	?printf("core start ...ok ",[]),
	ok .


%%
%% Local Functions
%%

