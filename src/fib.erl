-module(fib).
-compile(export_all).

fib(1)->
	1;
fib(2) ->
	1;
fib(N) ->
	fib(N-1)+fib(N-2).


fib_w(PID,N)->
    qijap:print(pid_to_list(self())),
	fib(N),
    qijap:print(pid_to_list(self())),
    %io:write({fib(N)}),
	PID!ok.

fib(N,M)->
	lists:map(fun(_)->
					  spawn(?MODULE,fib_w,[self(),N])
			  end, lists:seq(1,M)),
	lists:map(fun(_)->
					  receive
						  ok->ok
					  end
			  end,lists:seq(1,M)).
