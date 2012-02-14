%% during execution we encode and store the packets depending on their scheduler_id
%% when ?LIMIT of packets have arrived we write in the file groups of packets (each group containing packets with the same scheduler)

%% each packet starts with a byte related* to the time difference between this packet and the previous one
%% obviously this byte cannot be 0 so we use it to denote that the following packets belong to the next scheduler
%% if we read two 0 bytes that means that this was the last scheduler and the following packets belong to the first scheduler

%% normally one byte is used for the time difference but it same cases more bytes may be required
%% benchmarks should be done to determine the optimum default number

-module(gabi).
-compile(export_all).
%-exposts([open/1,store/2,close/1,add/3]).
-define(MAX_SIZE,4200).
-record(gabi,{file,
	      famdict,
	      data,
	      size,
	      coreN}).
	  

%% MFALib = [{F,A,[M]}]


open(NameData,NameFamdict,CoreN)->
    {ok,S}=file:open(NameData,[write,raw,compressed]),
    #gabi{file=S,famdict=famdict:new(NameFamdict),data=array:new(CoreN)}.

close(S)->
    SN=store(S),
    file:close(SN#gabi.file).

add(SID,Pack,S)->
    case S#gabi.size of
	?MAX_SIZE ->
	    add(SID,Pack,store(S));
	N ->
	    {NFamdict,Encoded} = encode(Pack,S#gabi.famdict),
	    NData = update(SID,Encoded,S#gabi.data),
	    #gabi{famdict=NFamdict,
		  data=NData,
		  size=N+1}
    end.

update(SID,Encoded,Data)->
    CurrentEntry=array:get(SID,Data),
    NewEntry=binary:list_to_bin(CurrentEntry,Encoded),
    array:set(SID,NewEntry,Data).

store(S)->
    L=array:to_list(S#gabi.data),
    L2=[<<0>>|lists:map(fun(X)->
				binary:list_to_bin([<<0>>,X])
			end,L)],
    file:write(S#gabi.file,binary:list_to_bin(L2)),
    S#gabi{data=array:new(S#gabi.coreN),
	   size=0}.
	   
    
%PID: default: 15, max: 28 in one node
% Common pack: 4 bytes
% 8 time bits, 1 flag bit=0, 1 io bit, 6+8 pid bits, 8 mda bits

% Extended pack: 5 bytes
% 8 time bits, 1 flag bit=1, 1 io bit, 6+8 pid bits, 8 mda bits, 
% 4 pid bits, 4 module pids

% Long time pack: 5 bytes
% 8 bits time, 1 flag bit=1, 21 bits time
% 8 bits 0

% Flag bit = 0 -> common
% Flag bit = 0 AND extra byte = 0  --> long time pack
% Flag bit = 0 AND extra byte \= 0 --> extended pack

encode({PID,IO,MFA,Time},Famdict,PrevTime)->
    case IO of
	in ->
	    IObit = 0;
	out ->
	    IObit = 1
    end,
    {NFamdict,MFAID} = encode(MFA,Famdict)},
    case {encode(PID),MFAID} of 
	{ {PID6,B3,PID4}, {B4,MFA4} }->
	    B5 = bits2bytes([PID4,MFA4]),
	    B2 = bits2bytes([IObit,[1],PID6]);
	{ {PID6,B3,PID4}, B4 }->
	    B5 = bits2bytes([PID4,[0,0,0,0]]),
	    B2 = bits2bytes([IObit,[1],PID6]);
	{ {PID6,B3}, {B4,MFA4} }->
	    B5 = bits2bytes([[0,0,0,0],MFA4]),
	    B2 = bits2bytes([IObit,[1],PID6]);
	{ {PID6,B3}, {B4} }->
	    B5 = [],
	    B2 = bits2bytes([IObit,0,PID6])
    end,
    case timediff(Time,PrevTime) of
	{B1}->
	    mergepack([B1,B2,B3,B4,B5]);
	{B1,LongPacks} ->
	    mergepack([B1,B2,B3,B4,B5,LongPacks])
    end.


	    
