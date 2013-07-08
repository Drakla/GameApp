%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-29
%%% Description: TODO: Add description to socket_io_response
%%% ==================================
-module(socket_io_response).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([response/2]).

%%
%% API Functions
%%


response(Socket , Bin)->
	gen_tcp:send(Socket, Bin) .
%%
%% Local Functions
%%

