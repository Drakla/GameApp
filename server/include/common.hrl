
-include("role.hrl").

-define(info(F, A), util:log("info", F, A, ?MODULE, ?LINE)).
-define(error_log(F, A),  util:log("error", F, A, ?MODULE, ?LINE)).
-define(wfile(F, A), util:log1("file", F, A, ?MODULE, ?LINE)).
-define(printf(F,A),util:log("printf", F, A, ?MODULE, ?LINE)).

%%数据库模块选择
-define(DB_MODULE, db_mysql).
%%Mysql数据库连接
-define(DB_POOL, mysql_conn).
%%数据库连接
-define(DB, mysql_conn).

-record(request , {
				   request_id,
				   request_type,
				   body
				   }).
-record(response,{
				  response_id ,
				  body
				  }).