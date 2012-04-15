-module(pcore).
-compile(export_all).
-define(DEFAULT_FNAME,trace_famdict).
-define(DEFAULT_FLAGS,[]).
-define(DEFAULT_GNAME,trace_gabi).
-define(DEFAULT_COREN,7).

%arrays are zero indexed. either fix the indexing or number of cores = real number of cores +1

% the tracing stops whenever end_tracing/0 is called
% default: when the given fun  returns the tracing stops
% if no_exit flag is used tracing wont stop when the given fun returns
% if end_tracing/0 is not called some of the trace might not be stored 
% erlang_tracing/1 ensures that the trace related to PID has been delivered

mant()->
    pcore:start(42,42,42).
test(M)->
    pcore:start(test,test,[M],[no_auto_stop]).

tw(M)->
    pcore:start(timer_wheel,wheel,[M]).

start(M,F,Args)->
    start(M,F,Args,?DEFAULT_GNAME,?DEFAULT_FNAME,?DEFAULT_COREN,?DEFAULT_FLAGS).

start(M,F,Args,Flags)->
    start(M,F,Args,?DEFAULT_GNAME,?DEFAULT_FNAME,?DEFAULT_COREN,Flags).

start(M,F,Args,GName,FName,CoreN,Flags)->
		HName=string:concat(atom_to_list(InName),atom_to_list(head)),
		{ok,F}=file:open(HName,[write]),
		io:write(F,CoreN),
		io:put_chars(F,"."),
		file:close(F),
    PIDs = lists:map(fun(X)-> spawn(pcore,start_tracer,[GName,FName,X,Flags,erlang:now()]) end, lists:seq(1,CoreN)),
    PID = spawn(?MODULE,master_tracer,[array:fix(array:from_list(PIDs))]),
    register(master_tracer,PID),
    erlang:trace(all,true,[running,
    			   scheduler_id,
    			   timestamp,
    			   {tracer,PID}]),
    apply(M,F,Args),
%    tester:start(PID),
    case lists:member(no_auto_stop,Flags) of
	true  -> ok;
	false -> stop()
		    
    end.

stop()->
    stop(all).
stop(PID)->
   erlang:trace_delivered(PID),
   erlang:trace(all,false,[]),
    master_tracer!exit.

% we manually implement delayed write since it appears to be faster 
% than delayed write performed with the default flag.
% also, the implementation might change in the future 
% ie using dets or other nodes (hence the wrappers)

start_tracer(PName,FName,N,_Flags,Now)->
    P=string:concat(atom_to_list(PName),integer_to_list(N)),
    F=string:concat(atom_to_list(FName),integer_to_list(N)),
    Pabi=pabi:open(P,F,Now),
    ?MODULE:tracer(Pabi,null).

    
tracer(Pabi,Prev)->
    receive
	exit->
	    pabi:close(Pabi),io:write(ok);
	{PID,IO,MFA,Time}->
	    P2 = {PID,IO,MFA,Time},
	    case IO of
		in -> 
		    NPrev = P2,
		    NPabi = Pabi;
		out ->
		    case Prev of
			null ->
			    NPrev = Prev,
			    NPabi = Pabi;
			_ ->
			    NPrev=null,
			    NPabi=pabi:add(Prev,P2,Pabi)
		    end
	    end,
	    tracer(NPabi,NPrev)
    end.

master_tracer(PIDs)->
    receive
	exit->
	    lists:map(fun(P)->P!exit end,array:to_list(PIDs));
	{trace_ts,PID,IO,SID,MFA,Time} ->
	    case MFA of
	    	{io,wait_io_mon_reply,2} ->
	    	    % io:wait_io_mon_reply/2 appears to only enter the schedulers (and never leave)
	    	    % possibly a problem with trace&io; temporarily we ignore those messages
	    	    %io:write('-----------iomon error'),io:nl(),
		    master_tracer(PIDs);
	    	_ ->
	    	    array:get(SID-1,PIDs)!{PID,IO,MFA,Time},  %0-indexed arrays
		    master_tracer(PIDs)
	    end
    end.
