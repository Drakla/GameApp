set NODE=0
set COOKIE=appserver
set NODE_NAME=app_server@127.0.0.1

set SMP=auto
set ERL_PROCESSES=102400
cd ../config
erl +P %ERL_PROCESSES% -smp %SMP% -pa ../ebin -name %NODE_NAME% -setcookie %COOKIE% -boot start_sasl -config sys  -s server server_start
echo start end

pause
