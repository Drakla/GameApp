%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-28
%%% Description: TODO: Add description to core_cfg
%%% ==================================
-module(core_cfg).

%%
%% Include files
%%
-include("common.hrl").

%%
%% Exported Functions
%%
-export([
		 init/0
		,start/1
		 ]).


-export([
		 get_global_value/1
		,global_save/1
		 ]).


%%
%% API Functions
%%

init()->
	case file:consult("init.cfg") of
		{ ok , [GlobalVarList , CoreKernelList , ModuleList ]} ->
			{ ok , [GlobalVarList , CoreKernelList , ModuleList ] } ;
		_Err ->
			?error_log("read init config error ~w" ,[_Err]) , 
			error
	end .		


start([Global , Core , Modules])->
	ok = init_global_var(Global) ,
	ok = init_core_srv(Core) ,
	ok = init_module_srv(Modules) ,
	?printf("core service start ...ok",[]),
	ok .
%%
%% Local Functions
%%

%% 
%% 初始化系统环境变量 
init_global_var([])->
	ok ;
init_global_var([{ K , V}|T])->
	global_save({ K , V } ) ,
	init_global_var(T) .

%%
%% 初始化核心服务模块
init_core_srv([])->
	ok ;
init_core_srv([{M , F , A} | T])->
	M:F(A) ,
	init_core_srv(T) .

%%
%% 初始化配置服务模块
init_module_srv([])->
	ok ;
init_module_srv([{M,F,A}|T])->
	M:F(A) ,	
	init_module_srv(T) .



%%获取全局变量
get_global_value(Key)->
	case ets:lookup(ets_global_param, Key) of
		[Value] -> Value ;
		_ ->
			?printf("can't find this global var ~w " , [Key]) ,
			undefined 
	end .

%% 保存全局变量的值
global_save(Value)->
	ets:insert(ets_global_param, Value ) ,
	ok .