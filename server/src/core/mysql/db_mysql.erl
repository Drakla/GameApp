%%%--------------------------------------
%%% @Module  : db_mysql
%%% @Author  : ygzj
%%% @Created : 2010.10.25
%%% @Description: mysql 数据库操作
%%%--------------------------------------
-module(db_mysql).
-include("common.hrl").
%% -include("record.hrl").
-compile([export_all]).

%% 插入数据表

insert(Server, Table_name, FieldList, ValueList) ->
  Sql = db_mysqlutil:make_insert_sql(Table_name, FieldList, ValueList),
  db_mysqlutil:execute({Sql, Server}).

insert(Table_name, FieldList, ValueList) ->
  Sql = db_mysqlutil:make_insert_sql(Table_name, FieldList, ValueList),
  db_mysqlutil:execute(Sql).

insert(Table_name, Field_Value_List) ->
  Sql = db_mysqlutil:make_insert_sql(Table_name, Field_Value_List),
  db_mysqlutil:execute(Sql).

insert_by_valuelist(Server, Table_name, ValueList) ->
  Sql = db_mysqlutil:make_insert_sql_by_valuelist(Table_name, ValueList),
  db_mysqlutil:execute({Sql, Server}).

%% 修改数据表(replace方式)
replace(Table_name, Field_Value_List) ->
	Sql = db_mysqlutil:make_replace_sql(Table_name, Field_Value_List),
	db_mysqlutil:execute(Sql).

replace(Server, Table_name, Field_Value_List) ->
	Sql = db_mysqlutil:make_replace_sql(Table_name, Field_Value_List),
    db_mysqlutil:execute({Sql, Server}).

%% 修改数据表(update方式)
update(Table_name, Field, Data, Key, Value) ->
	Sql = db_mysqlutil:make_update_sql(Table_name, Field, Data, Key, Value),
	db_mysqlutil:execute(Sql).
update(Table_name, Field_Value_List, Where_List) ->
	Sql = db_mysqlutil:make_update_sql(Table_name, Field_Value_List, Where_List),
	db_mysqlutil:execute(Sql).

update(Server, Table_name, Field_Value_List, Where_List) ->
	Sql = db_mysqlutil:make_update_sql(Table_name, Field_Value_List, Where_List),
	db_mysqlutil:execute({Sql, Server}).

%% 获取一个数据字段
select_one(Table_name, Fields_sql, Where_List, Order_List, Limit_num) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List, Order_List, Limit_num),
 	db_mysqlutil:get_one(Sql).
select_one(Table_name, Fields_sql, Where_List) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List),
	db_mysqlutil:get_one(Sql).

%% 获取一条数据记录
select_row(Table_name, Fields_sql, Where_List, Order_List, Limit_num) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List, Order_List, Limit_num),
 	db_mysqlutil:get_row(Sql).
select_row(Table_name, Fields_sql, Where_List) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List),
	db_mysqlutil:get_row(Sql).

select_row(Server, Table_name, Fields_sql, Where_List) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List),
	db_mysqlutil:get_row({Sql, Server}).

%% 获取记录个数
select_count(Table_name, Where_List) ->
	?DB_MODULE:select_row(Table_name, "count(1)", Where_List).

%% 获取所有数据
select_all(Table_name, Fields_sql, Where_List, Order_List, Limit_num) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List, Order_List, Limit_num),
	db_mysqlutil:get_all(Sql).

select_all(Server, Table_name, Fields_sql, Where_List) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List),
	db_mysqlutil:get_all({Sql, Server}).

select_all(Table_name, Fields_sql, Where_List) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List),
	db_mysqlutil:get_all(Sql).

%% 删除数据
delete(Table_name, Where_List) ->
	Sql = db_mysqlutil:make_delete_sql(Table_name, Where_List),
	db_mysqlutil:execute(Sql).

delete(Server,Table_name, Where_List) ->
	Sql = db_mysqlutil:make_delete_sql(Table_name, Where_List),
	db_mysqlutil:execute({Sql,Server}).

%% 事务处理
transaction(Fun) ->
	db_mysqlutil:transaction(Fun).



