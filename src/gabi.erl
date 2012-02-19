%% during execution we encode and store the packets depending on their scheduler_id
%% when ?LIMIT of packets have arrived we write in the file groups of packets (each group containing packets with the same scheduler)

%% each packet starts with a byte related* to the time difference between this packet and the previous one
%% obviously this byte cannot be 0 so we use it to denote that the following packets belong to the next scheduler
%% if we read two 0 bytes that means that this was the last scheduler and the following packets belong to the first scheduler

%% normally one byte is used for the time difference but it same cases more bytes may be required
%% benchmarks should be done to determine the optimum default number

-module(gabi).
-compile(export_all).
%-exports([open/1,store/2,close/1,add/4]).
-define(MAX_SIZE,inf).
-define(MAX_TIME,255).
-define(MAX_DUR, 63).
-record(gabi,{file,
	      famdict,
	      data,
	      size=0,
	      coreN,
	      prevtimes}).
	  

%% MFALib = [{F,A,[M]}]


open(NameData,NameFamdict,CoreN)->
    {ok,S}=file:open(NameData,[write,raw,compressed]),
    #gabi{file=S,
	  famdict=famdict:new(NameFamdict),
	  data=array:new([CoreN,{default,<<0:8>>}]),
	  coreN=CoreN,
	  prevtimes=array:new([CoreN,{default,erlang:now()}])}.


close(S)->
    SN=store(S),
    famdict:close(S#gabi.famdict),
    file:close(SN#gabi.file).

add(SID,Pack1,Pack2,S)->
    case S#gabi.size of
    	?MAX_SIZE ->
    	    add(SID,Pack1,Pack2,store(S));
    	N ->
    	    {Encoded,NFamdict,NPrevTime} = 
    		encode(Pack1,Pack2,
    		       S#gabi.famdict,
    		       array:get(SID,S#gabi.prevtimes)),
    	    NData = update(SID,Encoded,S#gabi.data),
    	    S#gabi{famdict=NFamdict,
    		  data=NData,
    		  size=N+1,
    		  prevtimes=array:set(SID,NPrevTime,S#gabi.prevtimes)}
    end.



update(SID,Encoded,Data)->
    CurrentEntry=array:get(SID,Data),
    NewEntry=[Encoded|CurrentEntry],
    array:set(SID,NewEntry,Data).

store(S)->
    L=array:to_list(S#gabi.data),
    file:write(S#gabi.file,binary:list_to_bin([<<1:7,0:7>>|L])),
    S#gabi{data=array:new([S#gabi.coreN,{default,<<0:8>>}]),
	   size=0}.


% Common pack ---> 5 bytes
% TimeIn: 8 bits  --cannot be 0--
% Duration: 6 bits --cannot be 0--
% MFAin = MFAout:  8 bits
% PID: 7+8 bits
% Flag

% if TimeIN = max -> 1 more byte [repeat]
% if Duration = max -> 1 more byte [repeat]
% Flag = 1 -> long PID (not supported yet)


% Core  Separator: <<0:8>>
% Write Separator: <<1:1,0:7>>
%% encode(P1,P2,F,P)->
%%     B=term_to_binary({P1,P2}),
%%     {B,F,P}.
encode({PID,in,MFAin,TimeIn},{PID,out,MFAout,TimeOut},Famdict,PrevTime)->
    {NPrevTime,TimeBytes} = time_encode(TimeIn,PrevTime),
    %NPrevTime = PrevTime,TimeBytes=0,
    PIDbytes = pid_encode(PID),
    %PIDbytes = 0,
    {MFAbytes,NFamdict,Fo,Fm} = mfa_encode(MFAin,MFAout,Famdict),
    %MFAbytes = 0, Fo=0, Fm=0, NFamdict=Famdict,
    DurationBytes = duration_encode(TimeOut,TimeIn,Fo,Fm),
    %DurationBytes = 0,
    %Final = binary:list_to_bin([DurationBytes,TimeBytes,PIDbytes,MFAbytes]),
    %Final = <<DurationBytes/binary, TimeBytes/binary,PIDbytes/binary,MFAbytes/binary>>,
    {[DurationBytes,TimeBytes,PIDbytes,MFAbytes],NFamdict,NPrevTime};
encode({PID1,in,MFA1,T1},{PID2,out,MFA2,T2},F,P) ->
    io:write('#--diff PID error--'),
    io:write({PID1,MFA1,PID2,MFA2,timediff(T2,T1)}),
    io:nl(),
    {<<0:8>>,F,P}.

pid_encode(PID)->
    BID=term_to_binary(PID),
    <<X:8,Y:8,_/binary>>=binary:part(BID,byte_size(BID),-7),
    <<0:1,X:7,Y:8>>.
	    
mfa_encode(MFA,MFA,Famdict)->
    {ID,NFamdict}=famdict:check(MFA,Famdict),
    case ID of
	{IDf,0}->
	    {<<IDf:8>>,NFamdict,0,0};
	{IDf,IDm}->
	    {<<IDf:8,IDm:4,0:4>>,NFamdict,0,1}
    end;
mfa_encode(MFAin,MFAout,Famdict)->
    {ID1,NFamdict1}=famdict:check(MFAin,Famdict),
    {ID2,NFamdict}=famdict:check(MFAout,NFamdict1),
    case {ID1,ID2} of
	{{ID1f,0},{ID2f,0}}->
	    {<<ID1f:8,ID2f:8>>,NFamdict,1,0};
	{{ID1f,ID1m},{ID2f,0}}->
	    {<<ID1f:8,ID2f:8,ID1m:4,0:4>>,NFamdict,1,1};
	{{ID1f,0},{ID2f,ID2m}}->
	    {<<ID1f:8,ID2f:8,0:4,ID2m:4>>,NFamdict,1,1};
	{{ID1f,ID1m},{ID2f,ID2m}}->
	    {<<ID1f:8,ID2f:8,ID1m:4,ID2m:4>>,NFamdict1,1,1}
    end.
	    
time_rec_encode(Time)->
    if Time<?MAX_TIME ->
	    [Time];
       true -> 
	    Left=time_rec_encode(Time-?MAX_TIME),
	    [?MAX_TIME|Left]
    end.

time_encode(TimeIn,PrevTime)->
    TimeList=time_rec_encode(timediff(TimeIn,PrevTime)),
    {TimeIn,binary:list_to_bin(TimeList)}.

duration_encode(TimeIn,TimeOut,Fo,Fm)->
    Duration=timediff(TimeOut,TimeIn),
	if Duration<?MAX_DUR ->
		<<Fo:1,Fm:1,Duration:6>>;
	   true->
		binary:list_to_bin([<<Fo:1,Fm:1,?MAX_DUR:6>>|
				    time_rec_encode(Duration-?MAX_DUR)])
	end.
		
timediff({M1,S1,U1},{M2,S2,U2})->
    ((M1-M2)*1000000+(S1-S2))*1000000+(U1-U2).


test()->
    P1={c:pid(0,42,0),in,{koko,lala,3}, {0,0,3}},
    P2={c:pid(0,42,0),out,{koko,lala,3},{0,0,5}},
    P3={c:pid(0,442,0),in,{kokop,lala,3},{0,1,5}},
    P4={c:pid(0,442,0),out,{kokop,lalak,3},{0,2,0}},
    P5={c:pid(0,442,0),in,{koko,lala,3},{0,2,1}},
    P6={c:pid(0,442,0),out,{koko,lala,3},{0,2,2}},
    G=open(gabitest,famdisktest,4),
    G1=add(1,P1,P2,G),
    G2=add(2,P3,P4,G1),
    G3=add(2,P5,P6,G2),
    close(G3).
