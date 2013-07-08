%%% ==================================
%%% Author: hanyq
%%% Created: 2013-6-6
%%% Description: TODO: Add description to connect_handler
%%% ==================================
-module(connect_handler).

%%
%% Include files
%%
-include("connecter.hrl").
%%
%% Exported Functions
%%
-export([handle/3 ,event/2 ,rpc/2,rpc_reply/2]).

%%
%% API Functions
%%
handle(login , _Request , Connecter )->
	{ ok , Connecter#connecter{ is_login = 1 } };
handle(rolelist , _Request , Connecter )->
	{ ok , Connecter } ;
handle(create , _Request , Connecter)->
	{ ok , Connecter } ;
handle(delete , _Request , Connecter)->
	{ ok , Connecter } ;
handle(enter , _Req , Connecter )->
	{ ok , Connecter } .


event(Connecter , Req )->
	gen_server:cast(Connecter#connecter.user_pid , {'socket_event' , Req }) ,
	{ ok , Connecter} .

rpc(Connecter , { M , F , A })->
	role_srv:cast(Connecter#connecter.user_pid , {cast, M, F, A} ) ,
	ok.

rpc_reply(Connecter , { M , F , A })->
	Reply = role_srv:call(Connecter#connecter.user_pid , {call, M, F, A} ) , 
	socket_io:send(Connecter#connecter.socket , Reply ) ,
	ok .

%%
%% Local Functions
%%

