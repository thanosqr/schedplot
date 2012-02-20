-module(ibap).
-compile(export_all).
-record(bytes,{file,
	       data,
	       left}).

-record(in,{value,
	    left,
	    file,
	    ones}).

-record(out,{file,
	     zoom,
	     coreID,
	     data,
	     size,
	     timestart,
	     datalength=0}).

-define(MAX_OUT,1000).
-define(READ_N,100000).
-define(MAX_DUR,63).
-define(MAX_TIME,255).
	       
get_packet(Bytes)->
    case get_byte(Bytes) of
	end_of_file ->
	    end_of_file;
	{Duration1,Bytes2}->
	    {Duration,Fo,Fm,Bytes3}=get_duration(Duration1,Bytes2),
	    {Time,Bytes4} = get_time(Bytes3),
	    {PID,Bytes5} = get_pid(Bytes4),
	    {MFA,Bytes6} = get_mfa(Fo,Fm,Bytes5),
	    {Bytes6,pack(Duration,Time,PID,MFA)}
    end.

get_byte(Bytes)->
    case Bytes#bytes.left of
	0 ->
	    case file:read(Bytes#bytes.file,?READ_N) of
		{ok,[D|Data]}->
		    {D,Bytes#bytes{left=erlang:length(Data),data=Data}};
		eof -> 
		    file:close(Bytes#bytes.file),
		    end_of_file
	    end;
	N ->
	    [Byte|Rest] = Bytes#bytes.data,
	    {Byte, Bytes#bytes{data=Rest,left=N-1}}
    end.


get_duration(Duration1,Bytes2)->    	       
    <<Fo:1,Fm:1,Duration:6>> = <<Duration1>>,
    if Duration<?MAX_DUR->
	    {Duration,Fo,Fm,Bytes2};
       Duration==?MAX_DUR ->
	    {DurationRec,Bytes3} = get_till_not_max(Duration,Bytes2,?MAX_DUR),
	    {DurationRec,Fo,Fm,Bytes3}
    end.

get_till_not_max(Acc,Bytes,Max)->
    {D,Bytes2}=get_byte(Bytes),
    if D<Max->
	    {Acc+D,Bytes2};
       D==Max ->
	    get_till_not_max(Acc+D,Bytes2,Max)
    end.


get_time(Bytes)->		
    {T,Bytes2} = get_byte(Bytes),
    if T<?MAX_TIME->
	    {T,Bytes2};
       T==?MAX_TIME ->
	    get_till_not_max(T,Bytes2,?MAX_TIME)
    end.

get_pid(Bytes)->
    {B1,Bytes1} = get_byte(Bytes),
    {B2,Bytes2} = get_byte(Bytes1),
    <<_:1,PID:15>> = <<B1:8,B2:8>>,
    {<<0:1,PID:15>>,Bytes2}.

get_mfa(Fo,Fm,Bytes)->
    case {Fo,Fm} of
	{0,0}->
	    get_byte(Bytes);
	{0,1} ->
	    {IDf,Bytes1}=get_byte(Bytes),
	    {IDm,Bytes2}=get_byte(Bytes1),
	    {{IDf,IDm},Bytes2};
	{1,0} ->
	    {MFA1,Bytes1}=get_byte(Bytes),
	    {MFA2,Bytes2}=get_byte(Bytes1),
	    {{MFA1,MFA2},Bytes2};
	{1,1} ->
	    {MFA1,Bytes1}=get_byte(Bytes),
	    {MFA2,Bytes2}=get_byte(Bytes1),
	    {Modulos,Bytes3}=get_byte(Bytes2),
	    {{MFA1,MFA2,Modulos},Bytes3}
    end.


pack(Duration,Time,_PID,_MFA)->
    {Time,Duration}.

get_times(Bytes)->
    get_packet(Bytes).


calc_val(end_of_file,_)->
    end_of_file;
calc_val(In,GU)->
    {Val,In2}=calc_val(In,GU,0),
    {Val,In2}.

calc_val(end_of_file,_N,Acc)->
    {Acc,end_of_file};    
calc_val(In,N,Acc)->
    case get_values(In,N) of
	{N2,NVal,In2}->
	    calc_val(refill(In2),N2,Acc+NVal);
	{Val,In2} ->
		{Acc+Val,In2}
    end.


get_values(In,N)->
    Left = In#in.left,
    if Left>=N ->
	    {N*In#in.value,In#in{left=Left-N}};
       Left<N ->
	    {N-Left,(N-Left)*In#in.value,In#in{left=0}}
    end.

refill(In)->
    case In#in.value of
	1 ->
	    case get_times(In#in.file) of
		end_of_file -> 
		    end_of_file;
		{Bytes,{Time,Duration}}->
		    In#in{file=Bytes,
			  left=Time,
			  value=0,
			  ones=Duration}
	    end;
	0 ->
	    In#in{left=In#in.ones,
		  value=1}
    end.


decode(In,Out,GU)->
    case calc_val(In,GU) of
	end_of_file->
	    close_out(Out);
	{Val,In2} ->
	    Out2=insert(Out,Val),
	    decode(In2,Out2,GU)
    end.

insert(Out,Val)->
    Size = Out#out.size,
    if Size > ?MAX_OUT ->
	    save(Out);
        Size =< ?MAX_OUT->
	    Out#out{data=[Val|Out#out.data],size=Size+1}
    end.

save(Out)->
%    io:write(Out#out.data),
    dets:insert(Out#out.file,{{Out#out.coreID, Out#out.zoom, Out#out.timestart},Out#out.data}),
    Out#out{data=[],timestart=Out#out.timestart+?MAX_OUT, size=0, datalength=Out#out.datalength+length(Out#out.data)}.


close_out(Out)->
    Out2=save(Out),
    dets:close(Out2#out.file),
    Out2#out.datalength.

open_out(OutName,CoreID)->
    {ok,File}=dets:open_file(OutName,[]),
    #out{file=File,
	     zoom=0,
	     coreID=CoreID,
	     data=[],
	     size=0,
	     timestart=0}.

open_in(InName,CoreID)->
    IName=string:concat(atom_to_list(InName),integer_to_list(CoreID)),
    {ok,File}=file:open(IName,[read,raw,compressed]),
    #in{file=#bytes{file=File,data=[],left=0},
	left=0,
	value=1,
	ones=0}.
    
decoder(OutName,InName,CoreID,GU,PID)->
    Out=open_out(OutName,CoreID),
    In =open_in(InName,CoreID), 
    PID!decode(In,Out,GU).
    

decode_all()->
    decode_all(decoded,trace_gabi,8,{1,6}).
decode_all(OutName,InName,GU,{FromCore,ToCore})->
    lists:map(fun(CoreID)->
		      spawn(ibap,decoder,[OutName,InName,CoreID,GU,self()])
	      end, lists:seq(FromCore,ToCore)),
    lists:max(lists:map(fun(_)->
				receive
			  N->
			      N
		      end end, lists:seq(FromCore,ToCore))).
    


generate_zoom_lvls(Dets,{FromCore,ToCore},MaxZoomOut)->
    lists:map(fun(OldZoom)->
		      lists:map(fun(CoreID)->
					Keys=dets:match(Dets,{{CoreID,OldZoom,'$3'},'_'}),
					SKeys=lists:sort(Keys),
					io:write(SKeys),io:nl(),
					traverse(Dets,CoreID,OldZoom,SKeys)
				end, lists:seq(FromCore,ToCore))
	      end, lists:seq(0,MaxZoomOut)).

traverse(Dets,CoreID,OldZoom,[[TimeIn1],[TimeIn2]|Keys])->
    [{_,Values1}]=dets:lookup(Dets,{CoreID,OldZoom,TimeIn1}),
    [{_,Values2}]=dets:lookup(Dets,{CoreID,OldZoom,TimeIn2}),
io:nl(),io:write('Val1: '),io:write(Values1),io:nl(),io:write('Val2: '),io:write(Values2),io:nl(),io:nl(),
    if length(Values1)==length(Values2)->
	    io:write(hellooooo),
	    Values=lists:zipwith(fun(X,Y)->(X+Y) div 2 end, Values1,Values2),
	    dets:insert(Dets,{{CoreID,OldZoom+1,TimeIn1},Values}),
	    traverse(Dets,CoreID,OldZoom,Keys);
       true -> ok
    end;
traverse(_Dets,_CoreID,_OldZoom,_Keys) ->
    ok.

    
    

analyze(InName,OutName,GU,{FromCore,ToCore})->
    Longest=decode_all(OutName,InName,GU,{FromCore,ToCore}),
io:write(Longest),
    MaxZoomLevel=erlang:trunc(math:log(Longest)/math:log(2))+1,
    {ok,Dets}=dets:open_file(OutName),
    generate_zoom_lvls(Dets,{FromCore,ToCore},MaxZoomLevel),
    dets:insert(Dets,{cores,ToCore-FromCore+1}),
    dets:close(Dets);
analyze(InName,OutName,GU,CoreN)->
    analyze(InName,OutName,GU,{1,CoreN}).
   
analyze(InName,OutName)->
    analyze(InName,OutName,8,{1,6}).


analyze()->
    analyze(trace_gabi,deleteme).
