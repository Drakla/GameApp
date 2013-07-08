-module(server).
-export([
            server_start/0,
        	server_stop/0,
            set_server_state/1,
            info/0,
            kick_users/0
        ]).

-include("server.hrl").
-include("common.hrl").

% 分别定义了需要开启的APP模块
-define(SERVER_APPS, [sasl, server]).


%%启动游戏服务器
server_start()->
    try
        ok = start_applications(?SERVER_APPS)
    after
        timer:sleep(100)
    end.

%%停止游戏服务器
server_stop() ->
    %%首先关闭外部接入，然后停止目前的连接，等全部连接正常退出后，再关闭应用
	kick_users(),    
    try stop_applications(?SERVER_APPS)
    catch
        T:Reason -> ?error_log( "T:~p~nReason:~p~n" , [T,Reason] )
    end,
	timer:sleep(120*1000),
    erlang:halt().


%% 关闭游戏入口
set_server_state(_IsOpen) ->
    ok.

%% 踢在线玩家下线
kick_users() ->
	set_server_state(0),
    %%L = ets:tab2list(r_user),
	L = [] ,
    kick_users(L).

kick_users([]) -> ok;
kick_users([H | T]) ->
   %% mod_user:stop((H#r_user.other_data)#r_user_other.pid,2),
    kick_users(T).


info() ->
    SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTot       = erlang:memory(total),
    io:format( "abormal termination:
                       ~n   Scheduler id:                         ~p
                       ~n   Num scheduler:                        ~p
                       ~n   Process count:                        ~p
                       ~n   Process limit:                        ~p
                       ~n   Memory used by erlang processes:      ~p
                       ~n   Memory allocated by erlang processes: ~p
                       ~n   The total amount of memory allocated: ~p
                       ~n",
                            [SchedId, SchedNum, ProcCount, ProcLimit,
                             ProcMemUsed, ProcMemAlloc, MemTot]),
      ok.

%%############辅助调用函数##############
manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun (App, Acc) ->
                    case Do(App) of
                        ok -> [App | Acc];
                        % 处理成功的情况下，将两参数合并到一个队列中
                        {error, {SkipError, _}} -> Acc;
                        % 处理失败，同时跳过原因则只返回原先带入的第二个参数队列
                        {error, Reason} ->
                            lists:foreach(Undo, Acc),
                            throw({error, {ErrorTag, App, Reason}})
                            % 如果为有原因的失败则回滚相关，并抛出异常
                    end
            end,
            % 以上为带入Iterate函数的第一个参数,此函数将带入的第3个列表参数列表中各项
            % 与第2个参数分别作为第一第二个参数
            [], Apps),
    ok.

start_applications(Apps) ->
    manage_applications(fun lists:foldl/3,
                        fun application:start/1, % 开启某一应用
                        fun application:stop/1,  % 结束某一应用
                        already_started,         % 当返回为already_started时表明进程已经开启，此时不做错误处理
                        cannot_start_application,
                        Apps).

stop_applications(Apps) ->
    F = fun() -> ok end,
    manage_applications(fun lists:foldr/3,
                        fun application:stop/1,
                        %%fun application:start/1,
                        F,
                        not_started,             % 当返回为not_started时表明进程还未开启，此时不做错误处理
                        cannot_stop_application,
                        Apps).

