%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-28
%%% Description: TODO: Add description to db
%%% ==================================
-module(db).

%%
%% Include files
%%
-include("common.hrl").
%%
%% Exported Functions
%%
-export([
		 start/1
		 ]).

%%
%% API Functions
%%
start([])->
	ok = init() ,
	?printf("db srv start ...ok",[]) ,
	ok .
	


%%
%% Local Functions
%%

init()->
	{ _ , Host } = core_cfg:get_global_value(mysql_host) ,
	{ _ , Port } = core_cfg:get_global_value(mysql_port) ,
	{ _ , User } = core_cfg:get_global_value(mysql_user) ,
	{ _ ,Password } =  core_cfg:get_global_value(mysql_password) ,
	{ _ , DB } = core_cfg:get_global_value(mysql_db) ,
	{ _ ,Encode} = core_cfg:get_global_value(mysql_encode) ,
	%io:format("db ~w/~w/~w/~w/~w/~w ~n",[Host, Port, User, Password, DB , Encode ]),
     mysql:start_link(mysql_dispatcher, ?DB_POOL, Host, Port, User, Password, DB,  fun mysql:log/4, Encode),
%% 	
	Count = 1 ,
    LTemp = lists:duplicate(Count, dummy),
    % 启动conn pool
    [begin
   		mysql:connect(mysql_dispatcher, ?DB_POOL, Host, Port, User, Password, DB, Encode, true)
    end || _ <- LTemp],
	ok .


