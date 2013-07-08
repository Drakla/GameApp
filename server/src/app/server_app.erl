%%%-----------------------------------
%%% @Module  : server_app
%%% @Description: 游戏应用服务器启动
%%%-----------------------------------
-module(server_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, []) ->
	
    ets:new(ets_global_param, [set, public, named_table]),

    %% 开启监控进程模块
    {ok, SupPid} = server_sup:start_link(),
	%% 时间进程
    %% 初始化配置
    { ok , Cfg } = core_cfg:init(),	
    % 开启具体的服务器相关操作进程
    core_cfg:start(Cfg) ,

    {ok, SupPid}.

stop(_State) ->
    %?DEBUG( "sd_server_app stop ...~p~n" , [self()] ),
	%server_handle:server_stop(),
    void.




