%%% ==================================
%%% Author: hanyq
%%% Created: 2013-4-23
%%% Description: TODO: Add description to util
%%% ==================================
-module(util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([unixtime/0 ,longunixtime/0]).
-export([log/5,log1/5]).
%%
%% API Functions
%%
log(T, F, A, Mod, Line) ->
    Msg = format(T, F, A, Mod, Line),	
    if
        T =:= "error" ->
            %db:log(Msg);
			skip;
        true ->
            skip
    end,
    io:format("~ts", [Msg]).

log1(T, F, A, Mod, Line) ->
    Msg = format(T, F, A, Mod, Line),
    {{Y, M, D}, {Hour,_,_}} = erlang:localtime(),
    Name = lists:concat(["../logs/", [integer_to_list(Y), "_", integer_to_list(M), "_", integer_to_list(D)], "_", integer_to_list(Hour)]),
    {ok, Fl} = file:open(Name, [write, append]),
    io:format(Fl, Msg, []),
    file:close(Fl), [].

%%
%% Local Functions
%%
%% 格式化日志信息
%% T = "error" | "info" | "debug" 类型
%% F = list() 格式
%% A = list() 参数
%% Mod = list() 模块名
%% Line = int() 所在行
format(T, F, A, Mod, Line) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    Date = lists:concat([Y, "/", M, "/", D, " ", H, ":", I, ":", S]),
    case Line of
           null -> erlang:iolist_to_binary(io_lib:format(lists:concat(["## ", T, " ~s ", F, "~n"]), [Date] ++ A));
       _ -> erlang:iolist_to_binary(io_lib:format(lists:concat(["## ", T, " ~s[~w:~w] ", F, "~n"]), [Date, Mod, Line] ++ A))
    end.



%% 取得当前的unix时间戳，单位为s
unixtime() ->
    {M, S, _} = mod_timer:now(),
    M * 1000000 + S.

longunixtime() ->
    {M, S, Ms} = mod_timer:now(),
    (M * 1000000000000 + S * 1000000 + Ms) div 1000.
