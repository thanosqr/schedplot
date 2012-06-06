-module(pcore).
-compile(export_all).
-include("hijap.hrl").


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
	T0 = erlang:now(),
	scarlet:init(FolderName,T0),
    PIDapplyT = spawn(?MODULE,wait_apply,[Fun]),    
	case lists:member(trace_tracer,Flags) of
		false -> PIDapply = PIDapplyT;
		true -> PIDapply = all
	end,

    PIDs = lists:map(fun(X)-> spawn(pcore,start_tracer,[FolderName,X,Flags,T0]) end, lists:seq(1,CoreN)),
    PID = spawn(?MODULE,master_tracer,[array:fix(array:from_list(PIDs))]),
    qutils:reregister(master_tracer,PID),
	TFlags = [running,scheduler_id,timestamp,{tracer,PID}],
	case lists:member(gc,Flags) of
		false ->
			erlang:trace(PIDapply,true,TFlags);
		true ->
			erlang:trace(PIDapply,true,[garbage_collection|TFlags])
	end,
	Agda=erlang:now(),
    PIDapplyT!{start,self()},
    receive
		apply_done->ok
    end,
io:write(timer:now_diff(erlang:now(),Agda)/1000000),
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
A=erlang:now(),
	Ref=erlang:trace_delivered(PID),
	receive 
		{trace_delivered, all, Ref}->
			ok
	end,
io:write({t,timer:now_diff(erlang:now(),A)}),io:nl(),
	erlang:trace(all,false,[all]),
io:write(erlang:trace_info(self(),flags)),
    master_tracer!{self(),exit},
	scarlet:close(),
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
		{exit,PID} ->
			c:flush(),
			pabi:close(Pabi),
			PID!ok
    end.

%io:wait_io_mon_reply/2 appears to only enter 
%the schedulers (and never leave)
%possibly a problem with trace&io;  we ignore those messages
master_tracer(PIDs)->
    receive
		{trace_ts,PID,IO,SID,MFA,Time} ->
			case MFA of
				{io,wait_io_mon_reply,2} ->
					master_tracer(PIDs);
				_ ->
					array:get(SID-1,PIDs)!{PID,IO,MFA,Time},  %0-indexed arrays
					master_tracer(PIDs)
			end;
		{trace_ts,PID,SE,_GC_Info,Time} ->
			case SE of
				gc_start->
					Msg = {PID,in,{gc,gc,0},Time};
				gc_end ->
					Msg = {PID,out,{gc,gc,0},Time}
			end,
			array:get(array:size(PIDs)-1,PIDs)!Msg,
			master_tracer(PIDs);
		{PID,exit}->

			[{_,NMQ}]=			erlang:process_info(self(),[message_queue_len]),
			io:write({nmq,NMQ}),io:nl(),
			fwd_rest(PIDs),
			lists:map(fun(P)->P!{exit,self()} end,array:to_list(PIDs)),
			lists:map(fun(_)->receive ok -> ok end end, array:to_list(PIDs)),
			PID!trace_stored
		end.
				
	
fwd_rest(PIDs)->	   
	%% [{_,NMQ}]=			erlang:process_info(self(),[message_queue_len]),
	%% if NMQ rem 1000 == 0 ->
	%% 		io:write(NMQ),io:nl();
	%%    true -> ok
	%% end,
	receive
		{trace_ts,PID,IO,SID,MFA,Time} ->
			case MFA of
				{io,wait_io_mon_reply,2} ->
					fwd_rest(PIDs);
				_ ->
					array:get(SID-1,PIDs)!{PID,IO,MFA,Time},  %0-indexed arrays
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
	after 10*1000 ->
			ok
	end.


create_folder(FolderName)->
	case file:make_dir(FolderName) of
		ok ->
			ok;
		{error, eexist} ->
			ok
	end.
