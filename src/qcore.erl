-module(qcore).
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

test(M)->
    qcore:trace(test,test,[M],[no_exit]).

tw(M)->
    qcore:trace(timer_wheel,wheel,[M]).

trace(M,F,Args)->
    trace(M,F,Args,?DEFAULT_GNAME,?DEFAULT_FNAME,?DEFAULT_COREN,?DEFAULT_FLAGS).

trace(M,F,Args,Flags)->
    trace(M,F,Args,?DEFAULT_GNAME,?DEFAULT_FNAME,?DEFAULT_COREN,Flags).

trace(M,F,Args,GName,FName,CoreN,Flags)->
    PID=spawn(?MODULE,trace_recorder,[GName,FName,CoreN,self()]),
    register(trace_recorder,PID),
    receive
	{PID}->
	    ok
    end,
    erlang:trace(all,true,[running,
			   scheduler_id,
			   timestamp,
			  {tracer,PID}]),
    T1=erlang:now(),
    apply(M,F,Args),
    T2 = erlang:now(),
    io:write(gabi:timediff(T2,T1)),io:nl(),
    case lists:member(no_exit,Flags) of
	true  -> ok;
	false -> end_tracing()
		    
    end.

end_tracing()->
    erlang:trace_delivered(all),
    erlang:trace(all,false,[]),
    trace_recorder!exit.
end_tracing(PID)->
    erlang:trace_delivered(PID),
    erlang:trace(all,false,[]),
    trace_recorder!exit.

% we manually implement delayed write since it appears to be faster 
% than delayed write performed with the default flag.
% also, the implementation might change in the future 
% ie using dets or other nodes (hence the wrappers)

trace_recorder(GabiName,FamdName,CoreN,PID)->
    Gabi=gabi:open(GabiName,FamdName,CoreN),
    Temps = array:new([CoreN,{default,null}]),
    PID!{self()},
    trace_recorder_(Gabi,Temps,0,0).
trace_recorder_(Gabi,Temps,RestTime,Packets)->

    receive
	exit->
	    io:write({RestTime,Packets}),
	    gabi:close(Gabi);
	{trace_ts,PID2,IO2,SID2,MFA2,Time2} ->
	    T1=erlang:now(),
	    case MFA2 of
	    	{io,wait_io_mon_reply,2} ->
	    	    % io:wait_io_mon_reply/2 appears to only enter the schedulers (and never leave)
	    	    % possibly a problem with trace&io; temporarily we ignore those messages
	    	    %io:write('-----------iomon error'),io:nl(),
	    	    NGabi = Gabi,
	    	    NTemps = Temps;
	    	_ ->
	    	    P2={PID2,IO2,MFA2,Time2},
	    	    case IO2 of
	    		in -> 
	    		    NTemps = array:set(SID2,P2,Temps),
	    		    NGabi = Gabi;
	    		out ->
	    		    P1=array:get(SID2,Temps),
	    		    case P1 of
	    			null ->
	    			    NTemps = Temps,
	    			    NGabi = Gabi;
	    			_ ->
	    			    NTemps=array:set(SID2,null,Temps),
	    			   % NGabi =Gabi
	    			   NGabi=gabi:add(SID2,P1,P2,Gabi)
	    		    end
	    	    end
	    end,
	    T2=erlang:now(),
	    trace_recorder_(NGabi,NTemps,gabi:timediff(T2,T1)+RestTime,Packets+1)
%	    trace_recorder_(NGabi,NTemps,RestTime,Packets+1)
    end.
