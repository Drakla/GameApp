%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-29
%%% Description: TODO: Add description to role_lib
%%% ==================================
-module(role_base_lib).

%%
%% Include files
%%
-include("common.hrl").
%%
%% Exported Functions
%%
-export([router/4]).
-export([load_role_info/1,unload_role_info/1]).
%%
%% API Functions
%%
router(MessageId , Role , ReqType , Data)->
	[H1, H2, _, _, _] = integer_to_list(MessageId),
	router_handler([H1,H2] , MessageId ,Role , ReqType , Data ) .


%%
%% Local Functions
%%
%% 路由
router_handler("13" , MessageId ,Role , ReqType , Data )->
	role_request:request( MessageId ,ReqType , Role , Data).




%% 
load_role_info(RoleId)->
	Role = list_to_tuple([r_role|db_role:get_role_by_id(RoleId)]) ,
	Role .

unload_role_info(Role)->
	ok.