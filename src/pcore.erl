-module(pcore).
-compile(export_all).
-include("hijap.hrl").

paradox_check(F,T)->
    %%  receive
    %% 	{PID1,in,SID1,MFA1} ->
    %% 	    receive
    %% 		{PID2,in,SID2,MFA2} ->
    %% 		    if SID1 == SID2 -> ok;
    %% 		       SID1=/= SID2 ->
    %% 			    io:write({PID1,PID2,SID1,SID2,MFA1,MFA2}),
    %% 			    io:nl()
    %% 		       end;
    %% 		_ ->
    %% 		    ok
		   
    %% 	    end;
    %% 	_-> ok
    %% end,
    %% paradox_check().
    receive
	{_PID,IO,SID,MFA,Time} ->
	    io:write(F,{IO,SID,MFA,timer:now_diff(Time,T)})
    end,
    paradox_check(F,T).

start(Fun,FolderName,CoreN,Flags)->
    create_folder(FolderName),
    HName=lists:concat([atom_to_list(FolderName),atom_to_list('/trace_gabi_header')]),
    {ok,FP}=file:open(HName,[write]),
    io:write(FP,CoreN),
    io:put_chars(FP,"."),
    io:nl(FP),
    io:write(FP,lists:member(gc,Flags)),
    io:put_chars(FP,"."),			 
    file:close(FP),
    {ok,F} = file:open(trace,[write]),
    PC = spawn(?MODULE,paradox_check,[F,erlang:now()]),

    PIDapplyT = spawn(?MODULE,wait_apply,[Fun]),    
	case lists:member(trace_tracer,Flags) of
		true -> PIDapply = PIDapplyT;
		false -> PIDapply = all
	end,
												 
    PIDs = lists:map(fun(X)-> spawn(pcore,start_tracer,[FolderName,X,Flags,erlang:now()]) end, lists:seq(1,CoreN)),
    PID = spawn(?MODULE,master_tracer,[array:fix(array:from_list(PIDs)),PC]),
    register(master_tracer,PID),
    case lists:member(gc,Flags) of
	false ->
	    erlang:trace(PIDapply,true,[running,
				      scheduler_id,
				      timestamp,
				      {tracer,PID}]);
	true ->
	    erlang:trace(PIDapply,true,[running,
				     garbage_collection,
				     scheduler_id,
				     timestamp,
				     {tracer,PID}])
	end,

    PIDapplyT!{start,self()},
    receive
	apply_done->ok
    end,
    case lists:member(no_auto_stop,Flags) of
		true  -> ok;
		false -> stop()

    end.

wait_apply(Fun)->
    receive
	{start,PID} ->
	    case Fun of
		{M,F,Args}->
		    erlang:apply(M,F,Args);
		Fun ->
		    erlang:apply(Fun,[])
	    end
    end,
    PID!apply_done.
	    

stop()->
    stop(all).
stop(PID)->
	erlang:trace_delivered(PID),
	erlang:trace(all,false,[]),
    master_tracer!{self(),exit},
	receive
		trace_stored->ok
	end.

% we manually implement delayed write since it appears to be faster 
% than delayed write performed with the default flag.
% also, the implementation might change in the future 
% ie using dets or other nodes (hence the wrappers)

start_tracer(FolderName,N,Flags,Now)->
    P=lists:concat([atom_to_list(FolderName),"/trace_gabi",integer_to_list(N)]),
    F=lists:concat([atom_to_list(FolderName),"/trace_famdict",integer_to_list(N)]),
    Pabi=pabi:open(P,F,Now,Flags),
    ?MODULE:tracer(Pabi,null).


tracer(Pabi,Prev)->
    receive
		exit->
			pabi:close(Pabi);
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
			tracer(NPabi,NPrev);
		PID ->
			pabi:close(Pabi),
			PID!ok
    end.


master_tracer(PIDs,PC)->

    receive
	{PID,exit}->
	    lists:map(fun(P)->P!self() end,array:to_list(PIDs)),
	    lists:map(fun(_)->receive ok -> ok end end, array:to_list(PIDs)),
	    PID!trace_stored;
	{trace_ts,PID,IO,SID,MFA,Time} ->
	    case MFA of
		{io,wait_io_mon_reply,2} ->
  % io:wait_io_mon_reply/2 appears to only enter the schedulers (and never leave)
  % possibly a problem with trace&io;  we ignore those messages
  %io:write('-----------iomon error'),io:nl(),
		    master_tracer(PIDs,PC);
		_ ->
		    array:get(SID-1,PIDs)!{PID,IO,MFA,Time},  %0-indexed arrays
		    PC!{PID,IO,SID,MFA,Time},
		    master_tracer(PIDs,PC)
	    end;
	{trace_ts,PID,SE,_GC_Info,Time} ->
	    case SE of
		gc_start->
		    Msg = {PID,in,{gc,gc,0},Time};
		gc_end ->
		    Msg = {PID,out,{gc,gc,0},Time}
	    end,
	    array:get(array:size(PIDs)-1,PIDs)!Msg,
	    master_tracer(PIDs,PC)
	end.
				
		   


create_folder(FolderName)->
	case file:make_dir(FolderName) of
		ok ->
			ok;
		{error, eexist} ->
			ok
	end.
