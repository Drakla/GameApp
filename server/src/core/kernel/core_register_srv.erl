%%% The core of the app: the server in charge of tracking processes.
-module(core_register_srv).
-behaviour(gen_server).

-export([start_link/0, stop/0, register/2, unregister/1, whereis/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%% We have two indexes: one by name and one by pid, for
%% MAXIMUM SPEED (not actually measured).
-record(state, {pid, name}).

%%%%%%%%%%%%%%%%%
%%% INTERFACE %%%
%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% Give a name to a process
register(Name, Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {register, Name, Pid}).

%% Remove the name from a process
unregister(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

%% Find the pid associated with a process
whereis(Name) ->
    case ets:lookup(regis_name, Name) of
        [{Name, {Pid,_}}] ->
            Pid;
        [] ->
            undefined
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_SERVER CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    %% Using gb_trees to store items. gb_trees generally have
    %% good overall performance.
    ets:new(regis_name, [named_table, protected, set, {read_concurrency, true}]),
    {ok, #state{pid = gb_trees:empty()}}.

handle_call({register, Name, Pid}, _From, S = #state{pid=P}) ->
    case ets:lookup(regis_name, Name) of
        [] ->
            Ref = erlang:monitor(process, Pid),
            true = ets:insert(regis_name, {Name, {Pid, Ref}}),
            {reply, ok, S#state{pid=gb_trees:insert(Pid, {Name,Ref}, P)}};
        [_] ->
            {reply, {error, name_taken}, S}
    end;
handle_call({unregister, Name}, _From, S = #state{pid=P}) ->
    case ets:lookup(regis_name, Name) of
        [{Name, {Pid,Ref}}] ->
            erlang:demonitor(Ref, [flush]),
            ets:delete(regis_name, Name),
            {reply, ok, S#state{pid=gb_trees:delete(Pid, P)}};
        [] ->
            {reply, ok, S}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, S = #state{pid=P}) ->
    {value, {Name, Ref}} = gb_trees:lookup(Pid, P),
    true = ets:delete(regis_name, Name),
    {noreply, S#state{pid = gb_trees:delete(Pid, P)}};
handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

