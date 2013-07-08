%%% -------------------------------------------------------------------
%%% Author  : hanyq
%%% Description :
%%%
%%% Created : 2013-3-29
%%% -------------------------------------------------------------------
-module(role_srv).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/1,cast/2 , call/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% API functions
%% ====================================================================
start_link(Args)->
	gen_server:start_link(?MODULE, Args, []) .

%% ====================================================================
%% Server functions
%% ====================================================================
cast(Pid ,Request)->
	gen_server:cast(Pid, Request).
call(Pid , Request)->
	gen_server:call(Pid, Request).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Args) ->
	[RoleId|_]=Args ,
	Role = role_base_lib:load_role_info(RoleId) ,
  	{ ok , Role }.

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
handle_call({call, M, F, Args}, _From, Role) ->
    try case M of
        undefined ->
            erlang:apply(F, [Role|Args]);
        _ ->
            erlang:apply(M, F, [Role|Args])
    end of
        {ok, Reply, NewRole} ->
            {reply, Reply, NewRole};
        {ok, Reply} ->
            {reply, Reply, Role};
        _ ->
            {reply, {false, bad_callback_return}, Role}
    catch
        T:W ->
            ?error_log("************ ~p:~p stacktrace:~p", [T, W, erlang:get_stacktrace()]),
            {reply, {error, W}, Role}
    end;
handle_call(_Request, _From, Role) ->
    Reply = ok,
    {reply, Reply, Role}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({cast, M, F, Args}, Role) ->
    try case M of
        undefined ->
            erlang:apply(F, [Role|Args]);
        _ ->
            erlang:apply(M, F, [Role|Args])
    end of
        {true, NewRole} ->
            {noreply, NewRole};
        _ ->
            {noreply, Role}
    catch
        T:W ->
            ?error_log("**** ~p:~p~n**** stacktrace:~p", [T, W, erlang:get_stacktrace()]),
            {noreply, Role}
    end;
handle_cast({ 'socket_event' , MessageId , ReqType , Data } , Role)->
	role_base_lib:router(MessageId , Role , ReqType , Data) ,
	{noreply , Role} ;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Local functions
%% --------------------------------------------------------------------

