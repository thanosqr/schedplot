-module(qcore).
-compile(export_all).
-define(DELAY,42).
-define(DEFAULT_NAME,qijap_trace).
-define(DEFAULT_FLAGS,[]).

% the tracing stops whenever end_tracing/0 is called
% default: when the given fun  returns the tracing stops
% if no_exit flag is used tracing wont stop when the given fun returns
% if end_tracing/0 is not called some of the trace might not be stored 
% erlang_tracing/1 ensures that the trace related to PID has been delivered

trace(M,F,Args)->
    trace(M,F,Args,?DEFAULT_NAME,?DEFAULT_FLAGS).

trace(M,F,Args,Name,Flags)->
    S=qabi:open(Name),
    PID=spawn(?MODULE,trace_recorder,[S,0]),
    register(trace_recorder,PID),
    erlang:trace(all,true,[running,
			   scheduler_id,
			   timestamp]),
    apply(M,F,Args),
    case lists:member(no_exit,Flags) of
	true  -> ok;
	false -> end_tracing()
		    
    end.

end_tracing()->
    erlang:trace_delivered(all),
    trace_recorder!exit.
end_tracing(PID)->
    erlang:trace_delivered(PID),
    trace_recorder!exit.

% we manually implement delayed write since it appears to be faster 
% than delayed write performed with the default flag.
% also, the implementation might change in the future 
% ie using dets or other nodes (hence the wrappers)

trace_recorder(S,?DELAY)->
    qabi:store(S),
    trace_recorder(S,0);
trace_recorder(S,N)->
    receive
	exit->
	    qabi:store(S),
	    qabi:close(S);
	{trace_ts,PID,IO,SID,MFA,Time} ->
	    trace_recorder(qabi:add(SID,{PID,IO,MFA,Time},S),N+1);
	_ ->
	    trace_recorder(S,N)
    end.
	    

open_storage(Name)->
    {ok,S}=file:open(Name,[write,raw,compressed]),
    S.

store(S,L)->
    file:write(S,<<term_to_binary(L),10>>).

close_storage(S)->
    file:close(S).
