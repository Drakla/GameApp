%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-29
%%% Description: TODO: Add description to client_response
%%% ==================================
-module(client_response).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([ response/3 ]).

%%
%% API Functions
%%
response(login , User , [Code , RoleList])->
	case Code of
		1 ->
			select ;
		0 ->
			fail
	end ;
	

response(_ , User , Param)->
	ok .


%%
%% Local Functions
%%

