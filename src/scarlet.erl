-module(scarlet).

-export([open/1, init/2, close/0, print/1, draw/7]).

-include("hijap.hrl").

-spec open(qijap:folder()) -> dict().
open(FolderName) ->
    F = FolderName ++ "/scarlet",
    {ok, S} = file:open(F, [read]),
    {ok, L} = io:read(S, ''),
    dict:from_list(L).

-spec init(qijap:folder(), erlang:timestamp()) -> 'ok'.
init(FolderName, T0) ->
    PID = spawn(fun() -> loop([], T0, FolderName) end),
    qutils:reregister(scarlet, PID).

-spec close() -> 'ok'.
close() ->
    scarlet ! {exit,self()},
    receive
	scarlet_done -> ok
    end.

-spec print(qijap:label()) -> 'ok'.
print(Label) ->
    T = erlang:now(),
    SID = erlang:system_info(scheduler_id),
    scarlet ! {T, {SID, Label}},
    ok.

draw(Paint, FromCore, {ZPos,XPos}, {Zoff,Xoff}, Width, D, VertZ) ->
    Z = round(math:pow(2,ZPos+Zoff+?DEF_GU-1)),  % edit
    X = (XPos+Xoff-?DETS_PACK_SIZE)*Z,
    L = dict:to_list(get_from_to(X,X+Width*Z,D)),
    plotter:scarlet(Paint, FromCore, L, Z, X, VertZ).

loop(Messages, T0, FolderName) ->
    receive
	{exit, PID} ->
	    F = FolderName ++ "/scarlet",
	    {ok, S} = file:open(F, [write]),
	    io:write(S, Messages),
	    io:put_chars(S, "."),			 
	    io:nl(S),
	    ok = file:close(S),
	    PID ! scarlet_done;
	{T, Label} ->
	    loop([{timer:now_diff(T,T0),Label}|Messages],T0,FolderName)
    end.

get_from_to(X1,X2,D) ->
    dict:filter(fun(X,_L) -> X1 =< X andalso X =< X2 end, D).
