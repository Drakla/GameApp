%%% -------------------------------------------------------------------
%%% Author  : hanyq
%%% Description :
%%%
%%% Created : 2013-6-6
%%% -------------------------------------------------------------------
-module(socket_io_acceptor).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {sock, ref }).

%% ====================================================================
%% API functions
%% ====================================================================
start_link(LSock  )->
	gen_server:start_link(?MODULE, {LSock }, []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init({LSock }) ->
	gen_server:cast(self(), accept),
    {ok, #state{ sock = LSock }}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(accept, State) ->
    accept(State);

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({inet_async, LSock, Ref, {ok, Sock}}, State = #state{sock=LSock, ref=Ref }) ->
    case set_sockopt(LSock, Sock) of
        ok -> ok;
        {error, Reason} -> exit({set_sockopt, Reason})
    end,
    start_client(Sock),
    accept(State);

handle_info({inet_async, LSock, Ref, {error, closed}}, State=#state{sock=LSock, ref=Ref}) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, State) ->
	 gen_tcp:close(State#state.sock),
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
accept(State = #state{sock=LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> {noreply, State#state{ref=Ref}};
        Error     -> {stop, {cannot_accept, Error}, State}
    end.

set_sockopt(LSock, Sock) ->
    true = inet_db:register_socket(Sock, inet_tcp),
    case prim_inet:getopts(LSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(Sock, Opts) of
                ok    -> ok;
                Error -> 
                    gen_tcp:close(Sock),
                    Error
            end;
        Error ->
            gen_tcp:close(Sock),
            Error
    end.

%% 开启客户端服务
start_client(Sock ) ->
    %%sd_tcp_client_sup实际为创建sd_reader进程
    {ok, Child} = supervisor:start_child(socket_io_client_sup, []),
    %%将SOCKET消息控制权转移到子进程中
    ok = gen_tcp:controlling_process(Sock, Child),
    %% ?DEBUG("start_client Socket:~w Child:~w", [inet:peername(Sock), Child]),
    Child ! {go, Sock }.