%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-29
%%% Description: TODO: Add description to role_request
%%% ==================================
-module(role_request).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([request/4 ]).

%%
%% API Functions
%%
request(request , type , Role , _Param)->
	%% 处理人物逻辑
	ok ;
request(13001 , Type , Role , [UserId])->
	
	role_response:response(13001, Type, Role) ,
	ok ;
request(_ , _Res , _Role , Param)->
	
	ok .


%%
%% Local Functions
%%

