-module(scarlet).
-compile(export_all).
-include("hijap.hrl").


init(FolderName,T0)->
    PID = spawn(?MODULE,loop,[[],T0,FolderName]),
    qutils:reregister(scarlet,PID).


close()->
    scarlet!{exit,self()},
    receive
	scarlet_done->
	    ok
    end.


print(Label)->
    scarlet!{erlang:now(),
	     {erlang:system_info(scheduler_id),Label}}.

loop(Messages,T0,FolderName)->
    receive
	{exit,PID}->
	    F=lists:concat([atom_to_list(FolderName),"/scarlet"]),
	    {ok,S}=file:open(F,[write]),
	    io:write(S,Messages),
	    io:put_chars(S,"."),			 
	    io:nl(S),
	    file:close(S),
	    PID!scarlet_done;
	{T,Label} ->
	    loop([{timer:now_diff(T,T0),Label}|Messages],T0,FolderName)
    end.
	    

open(FolderName)->
    F=lists:concat([atom_to_list(FolderName),"/scarlet"]),
    {ok,S}=file:open(F,[read]),
    {ok,L}=io:read(S,''),
    dict:from_list(L).

draw(Paint,{ZPos,XPos},{Zoff,Xoff},Width,D)->
    X = XPos+Xoff-?DETS_PACK_SIZE,
    Z=round(math:pow(2,ZPos+Zoff+?DEF_GU-1)),
    L=dict:to_list(get_from_to(X,X+Width*Z,D)),
    plotter:scarlet(Paint,L,Z,X).
    

get_from_to(X1,X2,D)->
    dict:filter(fun(X,_L)->
			   if (X1 =< X) and (X=<X2) ->
				   true;
			      true ->
				   false
			   end
		   end,D).
    
