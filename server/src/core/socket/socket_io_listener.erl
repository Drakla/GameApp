%%% ==================================
%%% Author: hanyq
%%% Created: 2013-6-6
%%% Description: TODO: Add description to socket_io_listener
%%% ==================================
-module(socket_io_listener).

-behaviour(gen_server).
%%
%% Include files
%%
%-include("socket_io.hrl").
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, true}, {exit_on_close, true}]).

%%
%% Exported Functions
%%
-export([start_link/2]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
%%
%% API Functions
%%
start_link(AcceptorCount, Port)->
	gen_server:start_link(?MODULE, {AcceptorCount, Port}, []).


%%
%% Local Functions
%%
init({AcceptorCount, Port})->
    process_flag(trap_exit, true),
	TCP_OPTIONS =?TCP_OPTIONS ,
    case gen_tcp:listen(Port, TCP_OPTIONS) of
        {ok, LSock} ->
            %%循环AcceptorCount次创建对应子进程的函数
            %%lists:duplicate 的作用在于拷贝处于第2参数的元组 N（第1参数）次
            lists:foreach(fun (_) ->
                                {ok, _APid} = supervisor:start_child(
                                                  socket_io_acceptor_sup, [LSock ])
                          end,
                          lists:duplicate(AcceptorCount, dummy)),
            %{ok, {LIPAddress, LPort}} = inet:sockname(LSock),
            {ok, LSock};
        {error, Reason} ->
            {stop, {cannot_listen, Reason}}
    end.	

handle_call(_Request, _From, State) ->
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %{ok, {IPAddress, Port}} = inet:sockname(State),
    gen_tcp:close(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.