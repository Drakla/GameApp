%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-29
%%% Description: TODO: Add description to client_request
%%% ==================================
-module(client_request).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
request(User , Req , Res)->
	
	remote:do_request(User , Res , role_request , login , [] ) , 
	
	ok .


%%
%% Local Functions
%%

