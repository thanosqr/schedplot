-module(ibag).
-compile(export_all).
-record(bytes,{file,
	       data,
	       core=0,
	       left}).
	       
get_packet(Bytes)->
    case get_byte(Bytes) of
	{<<0:8>>,Bytes2} ->
	    Bytes3 = Bytes2#bytes{core=Bytes2#bytes.core+1},
	    get_packet(Bytes3);
	{<<1:1,0:7>>,Bytes2} ->
	    Bytes3 = Bytes2#bytes{core=0},
	    get_packet(Bytes3);
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
	    get_byte(refill(Bytes));
	N ->
	    <<Byte:8,Rest/Binary>> = Bytes#bytes.data,
	    {Byte, Bytes#byte{data=Rest,left=N-1}}
    end.

refill(Bytes)->
    Bytes#bytes{left=?READ_N,
		data=file:read(Bytes#bytes.file,?READ_N)}.

get_duration(Duration1,Bytes2)->    	       
    <<Fo:1,Fm:1,Duration>> = Duration1,
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
    {B2,Bytes2} = get_byte(Bytes),
    <<_:1,PID:15>> = <<Bytes1:8,Bytes2:8>>,
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


get_unit(CurrentTime,Times,Bytes)->
    get_unit(CurrentTime,Times,Bytes,?UNIT_SIZE,0)
get_unit(CurrentTime,Times,Bytes,0,Acc)->
    {Acc/?UNIT_SIZE,CurrentTime,{TimeStart,Duration},Bytes};
get_unit(CurrentTime,Times,Bytes,N,Acc) -> 
    case get_one(CurrentTime,Times,Bytes) of
	end_of_file->
	    end_of_file;
	{Val,NCurrentTime,NTimes,NBytes} ->
	    get_one(NCurrentTime,NTimes,NBytes,N-1,Acc+Val)
    end.
get_one(CurrentTime,{TimeStart,Duration},Bytes)->
    if CurrentTime<TimeStart->
	    {0,CurrentTime+1,{TimeStart,Duration}};
       (CurrentTime>=Timestart) and (Duration>0)->
	    {1,CurrentTime+1,{TimeStart,Duration-1}};
       Duration==0->
	    =get_times(Bytes),
	    case get_times(Bytes) of
		end_of_file->
		    end_of_file;
		{NTimeStart,NDuration,NBytes} ->
		    get_one(CurrentTime,{NTimeStart,NDuration},NBytes)
	    end
    end.
