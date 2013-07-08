%%%------------------------------------
%%% @Module  : mod_kernel
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.06.30
%%% @Description: 核心服务
%%%------------------------------------
-module(core_srv).
-behaviour(gen_server).
-export([
            start_link/0
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").

-record(state, {}).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE, [], []).

init([]) ->
    %% 定义ets表
    ok = init_ets(),
    %% 加载静态数据
    ok = init_tpl(),
	%% 加载全局ets信息
	ok = init_global(),
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Info,_From, State) ->
    try
        do_call(Info,_From, State)
    catch
        _:Reason ->
            ?error_log("mod_kernel handle_call is exception:~w~n Info:~w get_stacktrace:~w",[Reason, Info, erlang:get_stacktrace()]),
            {reply, ok, State}
    end.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Info, State) ->
    try
        do_cast(Info, State)
    catch
        _:Reason ->
            ?error_log("mod_kernel handle_cast is exception:~w~n Info:~w get_stacktrace:~w",[Reason, Info, erlang:get_stacktrace()]),
            {noreply, State}
    end.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch
        _:Reason ->
            ?error_log("mod_kernel handle_info is exception:~w~n Info:~w get_stacktrace:~w",[Reason, Info, erlang:get_stacktrace()]),
            {noreply, State}
    end.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(normal, Status) ->
    {ok, Status}.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, Status, _Extra)->
    {ok, Status}.




%%---------------------do_call--------------------------------
do_call(Info, _, State) ->
    ?error_log("mod_kernel call is not match:~w",[Info]),
    {reply, ok, State}.


%%---------------------do_cast--------------------------------
do_cast(Info, State) ->
    ?error_log("mod_kernel cast is not match:~w",[Info]),
    {noreply, State}.


%%---------------------do_info--------------------------------
do_info(Info, State) ->
    ?error_log("mod_kernel info is not match:~w",[Info]),
    {noreply, State}.


%% ================== 私有函数 =================
%% 定义ETS表
init_ets() ->
	ets:new(ets_global,  [named_table, public, set] ),
    ok.

%% 加载静态数据
init_tpl() ->
    ok.

%%加载全局信息
init_global() ->
	ok.
