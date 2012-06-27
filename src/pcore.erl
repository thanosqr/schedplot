-module(pcore).

-export([start/4, stop/0]).

-include("hijap.hrl").

-spec start(function(), qijap:folder(), non_neg_integer(), qijap:start_flags()) -> 'ok'.
start(Fun, FolderName, CoreN, Flags) ->
    create_folder(FolderName),
    HName = FolderName ++ "/trace_gabi_header",
    {ok,FP} = file:open(HName, [write]),
    io:write(FP, CoreN),
    io:put_chars(FP, "."),
    io:nl(FP),
    io:write(FP, lists:member(gc, Flags)),
    io:put_chars(FP, "."),			 
    ok=file:close(FP),
    PIDapplyT = spawn(fun() -> wait_apply(Fun) end),    
    PIDapply = case lists:member(trace_all, Flags) of
		   false -> PIDapplyT;
		   true -> all
	       end,
    T0 = erlang:now(),
    scarlet:init(FolderName,T0),
    PIDs = lists:map(fun(X) -> 
			     spawn(fun() ->
					   start_tracer(FolderName,X,Flags,T0)
				   end) 
		     end, lists:seq(1,CoreN)),
    PID = spawn(fun()->
			master_tracer(array:fix(array:from_list(PIDs)),
				      array:get(array:size(PIDs)-1,PIDs),
				      lists:member(store_proc_info,Flags))
		end),
    qutils:reregister(master_tracer,PID),
    TFlags = [running,scheduler_id,timestamp,set_on_spawn,{tracer,PID}],
    case lists:member(gc,Flags) of
    	false ->
    	    erlang:trace(PIDapply,true,TFlags);
    	true ->
    	    erlang:trace(PIDapply,true,[garbage_collection|TFlags])
    end,
    PIDapplyT ! {start,self()},
    receive
	apply_done -> ok
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
    PID ! apply_done.

-spec stop() -> 'ok'.
stop() ->
    stop(all).

stop(PID) ->
    Ref = erlang:trace_delivered(PID),
    receive 
	{trace_delivered, all, Ref} -> ok
    end,
    erlang:trace(all, false, [all]),
    master_tracer!{self(),exit},
    scarlet:close(),
    receive
	trace_stored -> ok
    end.

%%%% %% we manually implement delayed write since it appears to be faster 
%%%% %% than delayed write performed with the default flag.
%%%% %% also, the implementation might change in the future 
%%%% %% ie using dets or other nodes (hence the wrappers)

start_tracer(FolderName, N, Flags, Now) ->
    NS = integer_to_list(N),
    P = FolderName ++ "/trace_gabi" ++ NS,
    F = FolderName ++ "/trace_famdict" ++ NS,
    Pabi = pabi:open(P, F, Now, Flags),
    tracer(Pabi, null).

tracer(Pabi, Prev) ->
    receive
	{IO,Msg} ->
	    if IO == in ; IO == gc_start ->
		    NPrev = {in, Msg},
		    NPabi = Pabi;
		IO == out ; IO == gc_end  ->
		    NPrev = null,
		    NPabi = case Prev of
				null ->
				    Pabi;
				_ ->
				    pabi:add(Prev, {out,Msg}, Pabi)
			    end
	    end,
	    tracer(NPabi, NPrev);
	{exit,PID} ->
	    ok = pabi:close(Pabi),
	    PID!ok
    end.

%%io:wait_io_mon_reply/2 appears to only enter 
%%the schedulers (and never leave)
%%possibly a problem with trace&io;  we ignore those messages
master_tracer(PIDs,GC_PID,Mode)->
    receive
	{trace_ts,PID,IO,SID,MFA,Time} ->
	    case MFA of
		{io,wait_io_mon_reply,2} ->
		    ok;
		_ ->
		    %% 0-indexed arrays
		    TracerPID = array:get(SID-1, PIDs),
		    TracerPID ! trim({IO,{PID,MFA,Time}},Mode)
	    end,
	    master_tracer(PIDs, GC_PID, Mode);

	{trace_ts,PID,SE,_GC_Info,Time} ->
	    GC_PID ! trim({SE, {PID, {gc,gc,0}, Time}}, Mode),
	    master_tracer(PIDs, GC_PID, Mode);

	{PID,exit} ->
	    fwd_rest(PIDs),
	    _ = [P ! {exit,self()} || P <- array:to_list(PIDs)],
	    _ = [receive ok -> ok end || _ <- array:to_list(PIDs)],
	    PID ! trace_stored
    end.		

trim(false, Msg) -> Msg;
trim(true, {InOut, {_, _, Time}}) -> {InOut, Time}.     
    
	
fwd_rest(PIDs)->	   
	receive
		{trace_ts,PID,IO,SID,MFA,Time} ->
			case MFA of
				{io,wait_io_mon_reply,2} ->
					fwd_rest(PIDs);
				_ ->
					array:get(SID-1,PIDs)!{PID,IO,MFA,Time},  %%0-indexed arrays
					fwd_rest(PIDs)
			end;
		{trace_ts,PID,SE,_GC_Info,Time} ->
			case SE of
				gc_start->
					Msg = {PID,in,{gc,gc,0},Time};
				gc_end ->
					Msg = {PID,out,{gc,gc,0},Time}
			end,
			array:get(array:size(PIDs)-1,PIDs)!Msg,
			fwd_rest(PIDs)
	after 420 ->
			ok
	end.


create_folder(FolderName)->
	case file:make_dir(FolderName) of
		ok ->
			ok;
		{error, eexist} ->
			ok
	end.


%%----------------------- Tests below ------------------------------

-ifdef(TEST).

mass_test()->
    lists:map(fun({FN,NT})->
		      start_test(FN,16,[],NT)
	      end,[{s1,1},{s10,10},{s100,100},{s300,300},{s600,600}]).
start_test(FolderName,CoreN,Flags,NTime)->
T00 = erlang:now(),
    create_folder(FolderName),
    HName=lists:concat([atom_to_list(FolderName),atom_to_list('/trace_gabi_header')]),
    {ok,FP}=file:open(HName,[write]),
    io:write(FP,CoreN),
    io:put_chars(FP,"."),
    io:nl(FP),
    io:write(FP,lists:member(gc,Flags)),
    io:put_chars(FP,"."),			 
    ok=file:close(FP),
   T0={0,0,0},
    scarlet:init(FolderName,T0),
    PIDs = lists:map(fun(X)-> spawn(
				fun()->
					start_tracer(FolderName,X,Flags,T0) 
				end) 
		     end, lists:seq(1,CoreN)),
    PID = spawn(fun()->
			master_tracer(array:fix(array:from_list(PIDs))) 
		end),
    qutils:reregister(master_tracer,PID),
    lists:foreach(fun(Core)->
		      PID!{trace_ts,Core,in,Core,{a,a,2},{0,0,0}},
		      PID!{trace_ts,Core,out,Core,{a,a,2},{0,NTime,0}}
	      end,lists:seq(1,16)),
    case lists:member(no_auto_stop,Flags) of
		true  -> ok;
		false -> stop()

    end,
    io:write({{time,NTime},qutils:round(timer:now_diff(erlang:now(),T00)/1000000,2)}).

-endif.
