-ifndef(CONNECTER_HRL ).
-define(CONNECTER_HRL , "connecter.hrl").

-define(HEADER_LENGTH , 5).
-define(HEART_TIMEOUT , 60000).
-define(TCP_TIMEOUT , 40000).
-define(FL_POLICY_REQ, <<"<poli">>).
-define(FL_POLICY_FILE, <<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>">>).


-record(connecter , {
					 user_id 
					 ,acc_name
					 ,socket
					 ,user_pid
					 ,is_login
					 ,platform
					}).

-endif.