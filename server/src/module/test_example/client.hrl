-record(r_role_tmp_data , {
						   role_pid ,
						   role_socket ,
						   parent_pid 
						   }).

-record(r_role , 
		{
			role_id ,
			accname ,		
			tmp_data = 	#r_role_tmp_data{}
		}).