%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-29
%%% Description: TODO: Add description to socket_io_request
%%% ==================================
-module(socket_io_request).

%%
%% Include files
%%
-include("common.hrl").
-include("proto.hrl").
%%
%% Exported Functions
%%
-export([ request/1]).

%%
%% API Functions
%%
request(<<MessageId:16 , Type:8 , Binary/binary>>)->
	Body = parse_binary(Binary , [] ) ,
	Request = #request{ request_id = MessageId  , request_type = Type , body = Body } , 
	request1(Request) .

%%
%% Local Functions
%%
request1(Request = #request{ request_id = RequestId} ) when RequestId == l0000 ->
	{ ok , login , Request} ;
request1(Request = #request{ request_id = RequestId} ) when RequestId == l0001 ->
	{ ok , rolelist , Request} ;
request1(Request = #request{ request_id = RequestId} ) when RequestId == l0002 ->
	{ ok , create , Request} ;
request1(Request = #request{ request_id = RequestId} ) when RequestId == l0003 ->
	{ ok , delete , Request} ;
request1(Request = #request{ request_id = RequestId} ) when RequestId == l0004 ->
	{ ok , enter , Request} ;
request1(Request)->
	{ok , Request}.
  
%%@doc 其他类型解析
%%
parse_binary(<<>> ,  DataList)->
	lists:reverse(DataList);
parse_binary(<<Type:8 , Bin/binary>> , List)->	
	{ ok , Data , RestBin } = parse_type(Type , Bin) ,
	parse_binary(RestBin , [Data|List]) .

%%
parse_type(?TYPE_INT_8 ,  <<Integer:8/integer-signed , Bin/binary>>)->
	{ ok , Integer , Bin};
parse_type(?TYPE_UINT_8 ,  <<Integer:8 , Bin/binary>>)->
	{ ok , Integer , Bin};
parse_type(?TYPE_INT_16 ,  <<Integer:16/integer-signed , Bin/binary>>)->
	{ ok , Integer , Bin};
parse_type(?TYPE_UINT_16 ,  <<Integer:16 , Bin/binary>>)->
	{ ok , Integer , Bin};
parse_type(?TYPE_INT_32 ,  <<Integer:32/integer-signed , Bin/binary>>)->
	{ ok , Integer , Bin};
parse_type(?TYPE_UINT_32 ,  <<Integer:32 , Bin/binary>>)->
	{ ok , Integer , Bin};
parse_type(?TYPE_BOOLEAN ,  <<Bool:8 , Bin/binary>>)->
	{ ok , Bool , Bin};
parse_type(?TYPE_STRING ,  Bin)->
	{ Str , RestBin } = read_string(Bin) ,
	{ ok , Str , RestBin};
parse_type(?TYPE_ARRAY , <<Len:16 , Bin/binary>>)->	 
	{  ok , ArrayList , RestBin } = read_array(Len , Bin , [] ),
	{ ok , ArrayList , RestBin };
parse_type(?TYPE_BINARY , <<Len:16 , Bin/binary>>)->	 
	<<Binary:Len/binary , RestBin/binary>> = Bin ,
	{ ok , Binary , RestBin };

parse_type(Type , _)->
	?printf("decode error_type data ~p",[Type]),
	{ok , [] , <<>> } .  

read_string(Bin) ->
    case Bin of
        <<Len:16, Bin1/binary>> ->
           	read_string1(Len , Bin1) ;
        _R1 ->
            {[],<<>>}
    end.

read_string1(Len , Bin)->
	case Bin of
		<<Str:Len/binary-unit:8, Rest/binary>> ->
			{binary_to_list(Str), Rest};
		_R1 ->
			{[],<<>>}
	end.	

%% 解析数组
read_array(0 , Bin , List)->
	DataList = lists:reverse(List) ,
	{ ok , DataList , Bin } ;
read_array(Len , <<Type:8 , Bin/binary>> , List)->
	{ ok , Data , RestBin } = parse_type(Type , Bin ) ,
	read_array(Len -1 , RestBin , [Data|List]) .