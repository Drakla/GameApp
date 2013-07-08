%%% ==================================
%%% Author: hanyq
%%% Created: 2013-6-5
%%% Description: TODO: Add description to socket_io_reader
%%% ==================================
-module(connect_reader).

%%
%% Include files
%%
-include("connecter.hrl").
-include("common.hrl").
%%
%% Exported Functions
%%
-export([start_link/0 ,init/0 ,loop/1]).

%%
%% API Functions
%%
start_link()->
	{ ok , proc_lib:spawn_link(?MODULE, init, []) } .


%%
%% Local Functions
%%
init()->
	 process_flag(trap_exit, true),
	 Connecter = #connecter{} ,
	 receive
		 { go , Socket}->
			 loop(Connecter#connecter{ socket = Socket }  ) 
	end .	 

%% 阻塞式监听socket ， 一直遍历从消息队列中读取收到的消息
loop(Connecter)->
	case get_recv_data(Connecter) of
		{ ok , NewConnecter} when is_record(NewConnecter , connecter ) ->
			loop(NewConnecter) ;
		{ok,flash_policy_finish} ->
			loop(Connecter) ;
		_Err ->
			exit(_Err) ,
			?error_log(" loop error_log ~w/",[_Err])
	end .		


%%=========================
%% local function
%%=========================
get_recv_data(Connecter = #connecter{ socket = Socket } )->
	Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
	receive
		 {inet_async, Socket, Ref, {ok, ?FL_POLICY_REQ}} ->
			Len = 23 - ?HEADER_LENGTH,
            async_recv(Socket, Len, ?TCP_TIMEOUT),
            lib_send:send_one(Socket, ?FL_POLICY_FILE),
			{ok,flash_policy_finish};
		{inet_async, Socket, Ref, {ok, <<EncryptMode:16,  IsZip:8 ,EncryptPackageLen:16 >>}} ->
			parse_request_data(Connecter , EncryptMode,IsZip,EncryptPackageLen) ;
		{inet_async, Socket, Ref, {error,timeout}} ->
			{error , timeout};
		{'EXIT', _Pid, Reason} when Reason =/= normal ->
			?error_log("get_recv_data error ~w",[Reason]) ,
			{error ,close};
		_Err ->
			{error ,close}
	end .
	
parse_request_data(Connecter , _EncryptMode,IsZip,EncryptPackageLen)->
	case parse_packet(Connecter , EncryptPackageLen , IsZip) of
		{ ok , Binary }->
			case socket_io_request:request(Binary) of
				{ ok ,  Event  , Req   }->
					connect_handler:handle(Event , Req, Connecter);
				{ ok , Req  }->
					connect_handler:event( Req ,Connecter );
				_Other ->
					?error_log("parse_request_data error ~w",[_Other]),
					{error,illegal_param}
			end ;					
		_Err ->
			_Err
	end .

%% 得到最终字节流
parse_packet(#connecter{socket = Socket},EncryptPackageLen,IsZip) ->
    case EncryptPackageLen > 0 of
        true ->
            Ref1 = async_recv(Socket, EncryptPackageLen, ?TCP_TIMEOUT),
            receive
               {inet_async, Socket, Ref1, {ok, Binary}} ->     
				   UnBinary = unzip_binary(IsZip , Binary),
                    { ok , UnBinary } ;
               _Other ->
                    ?error_log("parse_packet error....Cmd: BodyLength:~w Reason:~w",[EncryptPackageLen,_Other]),
                    {error, recv_data_error}
            end;
        false ->
            {error, recv_data_error}
    end.
%%解压二进制流
unzip_binary(IsZip , Binary)->
	case IsZip of
		0 ->Binary ;		
		1 ->zlib:uncompress(Binary)	
	end.

%% 接受信息
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.
	
	