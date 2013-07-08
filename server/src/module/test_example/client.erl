%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-29
%%% Description: TODO: Add description to client
%%% ==================================
-module(client).

%%
%% Include files
%%
-include("client.hrl").
%%
%% Exported Functions
%%
-export([start/1]).

%%
%% API Functions
%%
start(Accname)->
	
	{ ok , Pid } = client_role:start_link(Accname) ,
	
	ok .


%%
%% Local Functions
%%

