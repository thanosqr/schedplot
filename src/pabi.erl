%% during execution we encode and store the packets depending on their
%% scheduler_id when ?LIMIT of packets have arrived we write in the
%% file groups of packets (each group containing packets with the same
%% scheduler)

%% each packet starts with a byte related* to the time difference
%% between this packet and the previous one obviously this byte cannot
%% be 0 so we use it to denote that the following packets belong to
%% the next scheduler if we read two 0 bytes that means that this was
%% the last scheduler and the following packets belong to the first
%% scheduler

%% normally one byte is used for the time difference but in some cases
%% more bytes may be required benchmarks should be done to determine
%% the optimum default number

-module(pabi).

-export([open/4, add/3, close/1]).

-include("hijap.hrl").

-define(MAX_SIZE,10000).

-type trace_msg() :: {pid(), 'in' | 'out', mfa(), erlang:timestamp()}.

-record(pabi,{file,
	      famdict :: famdict:famdict(),
	      data,
	      size=0,
	      prevtime}).
-opaque pabi() :: #pabi{}.

%%%% MFALib = [{F,A,[M]}]

-spec open(file:filename(), file:filename(),
	   erlang:timestamp(), qijap:start_flags()) -> pabi().
open(NameData, NameFamDict, PrevTime, Flags) ->
    {ok, S} = file:open(NameData, [write, raw, compressed]),
    FamDict = case lists:member(trace_mfa, Flags) of
		  true ->
		      famdict:new(NameFamDict);
		  false ->
		      not_traced
	      end,
    #pabi{file = S,
	  famdict = FamDict,
	  data = [],
	  prevtime = PrevTime}.

-spec close(pabi()) -> 'ok' | {'error', atom()}.
close(S) ->
    SN = store(S),
    ok = famdict:close(SN#pabi.famdict),
    file:close(SN#pabi.file).

-spec add(trace_msg(), trace_msg(), pabi()) -> pabi().
add(Pack1, Pack2, S) ->
    case S#pabi.size of
    	?MAX_SIZE ->
    	    add(Pack1, Pack2, store(S));
    	N ->
    	    {Encoded,NFamdict,NPrevTime} = 
				encode(Pack1, Pack2, S#pabi.famdict, S#pabi.prevtime),
    	    NData = update(Encoded,S#pabi.data),
    	    S#pabi{famdict=NFamdict,
				   data=NData,
				   size=N+1,
				   prevtime=NPrevTime}
    end.

update(Encoded,Data) ->
    [Encoded|Data].

store(S) ->
    L = lists:reverse(S#pabi.data),
    %% L = S#pabi.data,
    B = binary:list_to_bin(L),
    ok = file:write(S#pabi.file,B),
    S#pabi{data = [], size = 0}.

%% Common pack ---> 5 bytes
%% TimeIn: 8 bits  --cannot be 0--
%% Duration: 6 bits --cannot be 0--
%% MFAin = MFAout:  8 bits
%% PID: 7+8 bits
%% Flag

%% if TimeIN = max -> 1 more byte [repeat]
%% if Duration = max -> 1 more byte [repeat]
%% Flag = 1 -> long PID (not supported yet)


%% Core  Separator: <<0:8>>
%% Write Separator: <<1:1,0:7>>
%%%% encode(P1,P2,F,P)->
%%%%     B=term_to_binary({P1,P2}),
%%%%     {B,F,P}.
encode({in,{PID,MFAin,TimeIn}},{out,{PID,MFAout,TimeOut}},Famdict,PrevTime)->
    PIDbytes = pid_encode(PID),			
    {MFAbytes,NFamdict,Fo,Fm} = mfa_encode(MFAin,MFAout,Famdict),
    TimeBytes = time_encode(TimeIn,PrevTime),
    DurationBytes = duration_encode(TimeIn,TimeOut,Fo,Fm),
    NPrevTime=TimeOut,
    {[DurationBytes,TimeBytes,PIDbytes,MFAbytes],NFamdict,NPrevTime};

encode({in,{TimeIn}},{out,{TimeOut}},Famdict,PrevTime)->
    TimeBytes = time_encode(TimeIn,PrevTime),
    DurationBytes = duration_encode(TimeIn,TimeOut,0,0),
    NPrevTime=TimeOut,
    {[DurationBytes,TimeBytes],Famdict,NPrevTime};

encode(_, _, F, P) ->
%   --diff PID error--
    {<<0:8>>,F,P}.

pid_encode(PID)->
    BID=term_to_binary(PID),
    <<X:8,Y:8,_/binary>> = binary:part(BID,byte_size(BID),-7),
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
    TimeList = time_rec_encode(timer:now_diff(TimeIn,PrevTime)),
    binary:list_to_bin(TimeList).

duration_encode(TimeIn,TimeOut,Fo,Fm)->
    Duration = timer:now_diff(TimeOut,TimeIn),
    if Duration < ?MAX_DUR ->
	    <<Fo:1,Fm:1,Duration:6>>;
       true->
	    binary:list_to_bin([<<Fo:1,Fm:1,?MAX_DUR:6>>|
				time_rec_encode(Duration-?MAX_DUR)])
    end.
