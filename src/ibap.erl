-module(ibap).
-compile(export_all).
-include("hijap.hrl").

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
	     key=1,
	     datalength=0}).

-define(MAX_OUT,?DETS_PACK_SIZE*?GU).
-define(READ_N,100000).


get_packet(Bytes)->
    case get_byte(Bytes) of
	end_of_file ->
	    end_of_file;
	{Duration1,Bytes2}->
	    case get_duration(Duration1,Bytes2) of
		{Duration,Fo,Fm,Bytes3}->
		    case get_time(Bytes3) of
			{Time,Bytes4} ->
			    case get_pid(Bytes4) of
				{PID,Bytes5} ->
				    case get_mfa(Fo,Fm,Bytes5) of
					{MFA,Bytes6} ->
					    {Bytes6,pack(Duration,Time,PID,MFA)};
					end_of_file -> end_of_file
				    end;
				end_of_file -> end_of_file
			    end;
			end_of_file -> end_of_file
		    end;
		end_of_file -> end_of_file
	    end
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

get_2_bytes(Bytes)->
    case get_byte(Bytes) of
	{Byte1,Bytes1}->
	    case get_byte(Bytes1) of
		{Byte2,Bytes2}->
		    {Byte1,Byte2,Bytes2};
		end_of_file ->
		    end_of_file
	    end;
	end_of_file ->
	    end_of_file
    end.

get_3_bytes(Bytes)->
    case get_byte(Bytes) of
	{Byte1,Bytes1}->
	    case get_byte(Bytes1) of
		{Byte2,Bytes2}->
		    case get_byte(Bytes2) of
			{Byte3,Bytes3} ->
			    {Byte1,Byte2,Byte3,Bytes3};
			end_of_file ->
			    end_of_file
		    end;
		end_of_file ->
		    end_of_file
	    end;
	end_of_file ->
	    end_of_file
    end.

get_duration(Duration1,Bytes2)->    	       
    <<Fo:1,Fm:1,Duration:6>> = <<Duration1>>,
    if Duration<?MAX_DUR->
	    {Duration,Fo,Fm,Bytes2};
       Duration==?MAX_DUR ->
	    case get_till_not_max(Duration,Bytes2,255) of
		{DurationRec,Bytes3} ->
		    {DurationRec,Fo,Fm,Bytes3};
		end_of_file ->
		    end_of_file
	    end
    end.

get_till_not_max(Acc,Bytes,Max)->
    case get_byte(Bytes) of
	{D,Bytes2} ->
	    if D<Max->
		    {Acc+D,Bytes2};
	       D==Max ->
		    get_till_not_max(Acc+D,Bytes2,Max)
	    end;
	end_of_file ->
	    end_of_file
    end.

get_time(Bytes)->		
    case get_byte(Bytes) of
	{T,Bytes2}->
	    if T<?MAX_TIME->
		    {T,Bytes2};
	       T==?MAX_TIME ->
		    get_till_not_max(T,Bytes2,?MAX_TIME)
	    end;
	end_of_file->
	    end_of_file
    end.

get_pid(Bytes)->
    case get_2_bytes(Bytes) of
	{B1,B2,Bytes2}->
	    <<_:1,PID:15>> = <<B1:8,B2:8>>,
	    {<<0:1,PID:15>>,Bytes2};
	end_of_file ->
	    end_of_file
    end.

get_mfa(Fo,Fm,Bytes)->
    case {Fo,Fm} of
	{0,0}->
	    get_byte(Bytes);
	{0,1} ->
	    case get_2_bytes(Bytes) of
		{IDf,IDm,Bytes2}->
		    {{IDf,IDm},Bytes2};
		end_of_file->
		    end_of_file
	    end;
	{1,0} ->
	    case get_2_bytes(Bytes) of
		{MFA1,MFA2,Bytes2}->
		    {{MFA1,MFA2},Bytes2};
		end_of_file->
		    end_of_file
	    end;

	{1,1} ->
	    case get_3_bytes(Bytes) of
		{MFA1,MFA2,Modulos,Bytes3}->
		    {{MFA1,MFA2,Modulos},Bytes3};
		end_of_file->
		    end_of_file
	    end
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
calc_val(In,GU,Acc)->
    case get_values(In,GU) of
	{GU2,NVal,In2}->
	    calc_val(refill(In2),GU2,Acc+NVal);
	{Val,In2} ->
	    {Acc+Val,In2}
    end.


get_values(In,GU)->
    Left = In#in.left,
    if Left>=GU ->
	    {GU*In#in.value,In#in{left=Left-GU}};
       Left<GU ->
	    {GU-Left,Left*In#in.value,In#in{left=0}}
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
    if Val > 8 ->io:write({wat});
       true -> ok
    end,
    Size = Out#out.size,
    if Size > ?MAX_OUT ->
	    save(Out);
       Size =< ?MAX_OUT->
	    Out#out{data=[Val|Out#out.data],size=Size+1}
    end.



save(Out)->
    dets:insert(Out#out.file,
		{{Out#out.coreID, Out#out.zoom, Out#out.key},Out#out.data}),
    Out#out{data=[],
	    key=Out#out.key+1,
	    size=0, 
	    datalength=Out#out.datalength+length(Out#out.data)}.


close_out(Out)->
    Out2=save(Out),
    Out2#out.datalength.

open_out(Dets,CoreID)->
    #out{file=Dets,
	 zoom=0,
	 coreID=CoreID,
	 data=[],
	 size=0}.

open_in(InName,CoreID)->
    IName=string:concat(InName,integer_to_list(CoreID)),
    {ok,File}=file:open(IName,[read,raw,compressed]),
    #in{file=#bytes{file=File,data=[],left=0},
	left=0,
	value=1,
	ones=0}.

decoder(FolderName,Dets,CoreID,GU,PID)->
    Out=open_out(Dets,CoreID),
    In =open_in(lists:concat([atom_to_list(FolderName),"/trace_gabi"]),CoreID), 
    PID!{decoder,decode(In,Out,GU)}.


decode_all(FolderName,Dets,GU,{FromCore,ToCore})->
    lists:map(fun(CoreID)->
		      spawn(ibap,decoder,[FolderName,Dets,CoreID,GU,self()])
	      end, lists:seq(FromCore,ToCore)),
    lists:max(lists:map(fun(_)->
				receive
				    {decoder,N}->
					N
				end end, lists:seq(FromCore,ToCore))).



generate_zoom_lvls(Dets,{FromCore,ToCore},MaxZoomOut)->
    lists:map(fun(OldZoom)->
		      lists:map(fun(CoreID)->
					Keys=dets:match(Dets,{{CoreID,OldZoom,'$3'},'_'}),
					SKeys=lists:sort(Keys),
					traverse(Dets,CoreID,OldZoom,SKeys)
				end, lists:seq(FromCore,ToCore))
	      end, lists:seq(0,MaxZoomOut)).

traverse(Dets,CoreID,OldZoom,[[Key1],[Key2]|Keys])->
    [{_,Values1}]=dets:lookup(Dets,{CoreID,OldZoom,Key1}),
    [{_,Values2}]=dets:lookup(Dets,{CoreID,OldZoom,Key2}),
    Values=lists:append(zoom_out(Values1),zoom_out(Values2)),
    dets:insert(Dets,{{CoreID,OldZoom+1,Key2 div 2},Values}),
    traverse(Dets,CoreID,OldZoom,Keys);
traverse(Dets,CoreID,OldZoom,[[Key1]])->
    [{_,Values1}]=dets:lookup(Dets,{CoreID,OldZoom,Key1}),
    Values=zoom_out(Values1),
    dets:insert(Dets,{{CoreID,OldZoom+1,(Key1+1) div 2},Values});
traverse(_Dets,_CoreID,_OldZoom,_Keys) ->
    ok.


zoom_out([H1,H2|T])->
    [round((H1+H2)/2)|zoom_out(T)];
zoom_out([H]) ->
    [round(H/2)];
zoom_out([]) ->
    [].




analyze(FolderName,GU,{FromCore,ToCore})->
    {ok,Dets}=dets:open_file(lists:concat(
			       [atom_to_list(FolderName),
				"/analyzed_trace"]),[]),
    dets:delete_all_objects(Dets),
    Longest=decode_all(FolderName,Dets,GU,{FromCore,ToCore}),
    MaxZoomLevel=erlang:trunc(math:log(Longest)/math:log(2))+1,
    dets:insert(Dets,{init_state,MaxZoomLevel,ToCore-FromCore+1}),
    generate_zoom_lvls(Dets,{FromCore,ToCore},MaxZoomLevel),
    dets:insert(Dets,{cores,ToCore-FromCore+1}),
    dets:close(Dets);
analyze(FolderName,GU,CoreN)->
    analyze(FolderName,GU,{1,CoreN}).

analyze(FolderName)->
    HName=string:concat(atom_to_list(FolderName),"/trace_gabi_header"),
    {ok,F}=file:open(HName,[read]),
    {ok,CoreN} = io:read(F,''),
    file:close(F),
    analyze(FolderName,?GU,CoreN).


analyze()->
    analyze(?DEFAULT_FOLDER_NAME).

