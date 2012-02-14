-module(qijap).
-compile(export_all).
-define(ETS_BATCH,42).



%%------------------------------------------%%
%%-----------------INTERFACE----------------%%
%%------------------------------------------%%

start()->
    spawn(qijap,init,[qijap_trace]).

stop()->
    qijap!exit.

display(To,VZ)->
    display(qijap_trace,To,VZ).
display(File,To,VertZoom)->
    {ok,L}=ets:file2tab(File),
    T=trace_conversion(L),
    CoreLengths=plot_cores(16,{0,To,800,T}),
    twst:init(CoreLengths,VertZoom).

    
	  

%%------------------------------------------%%
%%-------------------TRACE------------------%%
%%------------------------------------------%%



init(Filename)->
    T = ets:new(trace,[bag]),
    erlang:trace(all,true,
      [running,
       scheduler_id,
       timestamp]),
    qijap:save_to_ets(T,Filename).

save_to_ets(T,Filename)->
    receive
	trash->ets:delete(T);
	exit-> ets:tab2file(T,Filename);
	{trace_ts,PID,IO,SID,MFA,Time}->
	    ets:insert(T,{PID,IO,SID,MFA,Time}),
	    save_to_ets(T,Filename);
	_ -> save_to_ets(T,Filename)
    end.



%%------------------------------------------%%
%%-------------TRACE CONVERSION-------------%%
%%------------------------------------------%%

%% TODO: optimized tables for read/write?
    
%{PID,IO,SID,MFA,Time}

trace_conversion(T)->
    Temp = trace_to_temp(T),
    temp_to_final(Temp).

trace_to_temp(T)->
    Temp=ets:new(temp,[ordered_set]),
    H=ets:match_object(T,'_',?ETS_BATCH),
    trace_to_temp(T,Temp,H).

trace_to_temp(_T,Temp,'$end_of_table')->
    Temp;
trace_to_temp(T,Temp,{H,Cont})->
    lists:foreach(fun({PID,IO,SID,MFA,TIME})->
			  ets:insert(Temp,{{SID,TIME},PID,IO,MFA})
		  end,H),
    H2 = ets:match_object(Cont),
    trace_to_temp(T,Temp,H2).
		

temp_to_final(T)->
    Final=ets:new(final,[ordered_set]),
    H=ets:match_object(T,'_',?ETS_BATCH),
    [{{_,TimeZero},_,_,_}]=ets:lookup(T,ets:first(T)),
    temp_to_final(T,Final,H,TimeZero).

temp_to_final(_T,Final,'$end_of_table',_TimeZero)->
    Final;
temp_to_final(T,Final,{H,Cont},TimeZero)->
    zipself(Final,H,TimeZero),
    H2 = ets:match_object(Cont),
    temp_to_final(T,Final,H2,TimeZero).

zipself(Final,[],_TimeZero)->
    Final;
zipself(Final,[{{SID,TimeIn},PID,in,MFA1},{{SID,TimeOut},PID,out,MFA2}|T],TimeZero)->
    TimeInZ=time_diff(TimeIn,TimeZero),
    TimeOutZ=time_diff(TimeOut,TimeZero),
    ets:insert(Final,{{SID,TimeInZ},PID,TimeOutZ,TimeOutZ-TimeInZ,{MFA1,MFA2}}),
    zipself(Final,T,TimeZero);
zipself(Final,[_UnmatchedEntry|T],TimeZero)->
    zipself(Final,T,TimeZero).
    

time_diff({M2,S2,U2},{M1,S1,U1})->
    ((M2-M1)*1000000+(S2-S1))*1000000+(U2-U1).


%%------------------------------------------%%
%%---------------GUI REQUESTS---------------%%
%%------------------------------------------%%

plot_cores(Cores,Data)->
    plot_cores(Cores,1,Data).
plot_cores(Cores,Cores,_)->
    [];
plot_cores(Cores,Core,Data)->
    [plot(Core,Data)|plot_cores(Cores,Core+1,Data)].

plot(Core,{From,To,Width,Table})->
    Step = (To-From)/Width,
    pixel_values(Core,From,To,Step,Table).


pixel_values(_Core,From,To,_Step,_Table) when From>=To->	    
    [];
pixel_values(Core,From, To, Step,Table) when From<To->	
    Next=From+Step,
    ActivityPerc=active_time(Core,Table,From,Next)/Step,
    [ActivityPerc|pixel_values(Core,Next,To,Step,Table)].

active_time(Core,Table,From,To)->
     case ets:select(Table,[{
		       { {Core,'$1'},'_','_','_','_'},
                       [{'>=','$1',From}],
                       ['$_'] }], 1) 
     of
	 '$end_of_table'->
	     0;
	 {[{FirstKey,_,_,_,_}],_Cont}->
	     Acc=check_ending_proc(Core,Table,FirstKey,From),
	     active_time(Core,Table,FirstKey,To,Acc)
     end.

%why does ets:lookup/2 return a list even if table is an ordered set?
active_time(Core,Table,Current,Limit,Acc)->
    case Current of
	'$end_of_table'->
	    Acc;
	_ -> 
	    [{{Core,TimeIn},_PID,TimeOut,Duration,_MFA}]=ets:lookup(Table,Current),
	    if TimeIn>=Limit ->
		    Acc;
	       TimeOut<Limit ->
		    active_time(Core,Table,ets:next(Table,Current),Limit,Acc+Duration);
	       TimeOut>=Limit ->
		    Acc+Limit-TimeIn
	    end
    end.
	    
	

check_ending_proc(Core,Table,FirstKey,From)->
    case ets:prev(Table,FirstKey) of
	'$end_of_table' ->
	    0;
	PrevKey ->
	    case ets:lookup(Table,PrevKey) of
		[{{Core,_TimeIn},_PID,TimeOut,_Duration,_MFA}] ->
		    if TimeOut>From ->
			    TimeOut-From;
		       TimeOut=<From ->
			    0
		    end;
		_ -> 0
	    end
    end.





%%------------------------------------------%%
%%------------------TESTING-----------------%%
%%------------------------------------------%%

timer(Arg)->
      T1=erlang:now(),
    timer_wheel:wheel(Arg),
      T2=erlang:now(),
    timer_wheel:no_wheel(Arg),
      T3=erlang:now(),

    P=qijap:start(),
      T4a=erlang:now(),
    timer_wheel:wheel(Arg),
      T4=erlang:now(),
    P!trash,
    PP=qijap:start(),
      T5a=erlang:now(),
    timer_wheel:no_wheel(Arg),
      T5=erlang:now(),
    PP!trash,

    {time_diff_sec_rev(T1,T2),
     time_diff_sec_rev(T2,T3),
     time_diff_sec_rev(T4a,T4),
     time_diff_sec_rev(T5a,T5)
    }.

%% testall()->
%%     X=lists:map(fun(X)->
%% 		      timer(X)
%% 	      end,[500,750,1000,1250,1500]),
%%     X.
    
     

time_diff_sec_rev({M1,S1,_U1},{M2,S2,_U2})->
    (M2-M1)*1000000+(S2-S1).

%733314
avg_tab(Tab)->
    ets:foldl(fun({_,_,_,X,_},{_,_,_,Y,_})-> X+Y end, 0,Tab)
