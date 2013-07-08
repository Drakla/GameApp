%%% ==================================
%%% Author: hanyq
%%% Created: 2013-6-29
%%% Description: TODO: Add description to db_role
%%% ==================================
-module(db_role).

%%
%% Include files
%%
-include("common.hrl").
%%
%% Exported Functions
%%
-export([get_role_by_id/1]).

%%
%% API Functions
%%
get_role_by_id(RoleId)->
	?DB_MODULE:select_row(t_role, "*", [{role_id, RoleId}]).


%%
%% Local Functions
%%

