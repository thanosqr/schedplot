-module(test).
-compile(export_all).

test(M)->
%    erlang:trace(all,true,[running,timestamp,scheduler_id]),
    lists:map(fun(X)->spawn(test,fibw,[X]) end,lists:seq(1,M)).
%    rec([1],1,1).

tin()->
    spawn(test,t,[]).
t()->
    erlang:trace(all,true,[running,scheduler_id,timestamp]),
%    spawn(test,fibw,[30]),
    lists:map(fun(X)->spawn(test,fibw,[X]) end,lists:seq(1,38)),
    tt([]).
tend(P)->
    spawn(erlang,trace_delivered,[all]),
    P!{exit,self()},
    receive
	L ->
	     L
    end.
tt(L)->
    receive
	{exit,PID}->
%	    io:write(L),
	    PID!L;
	{trace_ts,_PID1,_IO1,_SID1,{io,wait_io_mon_reply,2},_Time1}->
	    tt(L);
	{trace_ts,PID1,IO1,SID1,MFA1,Time1}->
	    tt([{PID1,IO1,SID1,MFA1,gabi:timediff(Time1,{0,0,0})}|L])
    end.
    
search([])->ok;
search([{_PID1,IO,SID,_MFA1,_Time1},{PID2,IO,SID,MFA2,Time2}|T])->
    io:write(errr),
    io:nl(),
    search([{PID2,IO,SID,MFA2,Time2}|T]);
search([_H|T]) ->
    search(T).

rec(L,N,C)->
    receive
	{_,_,_,3,_,{_M,S,U}}->
	    T = S*1000000+U,
	    {NL,MK}=insert(T,L),
	    if (MK>0)->
		    io:write({MK,N,round(C/(1000*N)),S,U}),
		    io:nl(),
		    NL;
	       true-> ok
	    end,
	    rec(NL,N+1,C+MK);
	_->rec(L,N,C)
    end.

    
insert(X,[])->{[X],0};
insert(X,[S|Ss])->
    if X>=S ->
	    {[X,S|Ss],0};
       true ->
	    {R,N}=insert(X,Ss),
	    {[S|R],N+1}
    end.
	  
			


fibw(X)->
    fib(X),
    io:write({X,fib(X)}),io:nl().
fib(0)->
    1;
fib(1)->
    1;
fib(X) ->
    fib(X-1)+fib(X-2).



% L=[3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 66, 115, 206, 296, 335, 454, 707, 1082, 1659, 2500, 3817, 5768, 8673, 12986, 19347, 28600, 41913, 60744, 86881, 122102, 167703, 223042, 282735, 331028]
