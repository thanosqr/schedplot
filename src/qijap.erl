-module(qijap).
-compile(export_all).
-include("hijap.hrl").

view(FolderName)->
	uds:init(FolderName).

view()->
	view(?DEFAULT_FOLDER_NAME).

analyze(FolderName)->
	ibap:analyze(FolderName).

analyze()->
	analyze(?DEFAULT_FOLDER_NAME).

stop()->
	pcore:stop().

start(Fun,FolderName,Flags)->
	case lists:member(gc,Flags) of
		false->
			pcore:start(Fun,FolderName,
						erlang:system_info(schedulers),Flags);
		true ->
			pcore:start(Fun,FolderName,
						erlang:system_info(schedulers)+1,Flags)
	end.

start(Fun,Flags) when erlang:is_list(Flags) ->
	start(Fun,?DEFAULT_FOLDER_NAME,Flags);
start(Fun,FolderName) ->
	start(Fun,FolderName,[]).

start(Fun)->
	start(Fun,?DEFAULT_FOLDER_NAME,[]).

print(Label)->
    scarlet:print(Label).

%%--------------------------------------------------------------%%	
%%---------------------------- testing -------------------------%%
%%--------------------------------------------------------------%%	

list(X)->
	start({lists,seq,[1,X*1000*100]},[trace_tracer]).

nlist(X)->
	start({lists,seq,[1,X*1000*100]}).

t()->
	{ok,S} = dets:open_file("qijap_profile/analyzed_trace"),
	S.

r(S,X)->
	lists:reverse(lists:sort(dets:match(S,{{X,0,'$1'},'_'}))).

seq(X)->
    start({?MODULE,derps,[X*1000000]}).

derps(X)->
    qijap:print(0),
    derp(X).
derp(0)->
%    qijap:print(o),
    ok;
derp(X)->
    if X div 100 == 0 ->
%	    herp(1000000),
	    qijap:print(0);
       true -> ok
    end,
    derp(X-1).


herp(0)->
    ok;
herp(X) ->
    herp(X-1).

% { {PID = SID, TimeIn}, TimeOut,PID}
search()->
    {ok,S}=dets:open_file("saved/p220",[]),
    dets:traverse(S,fun({ {SID,{Tin1,Tin2,Tin3}}, {Tout1,Tout2,Tout3},PID})->
			    case dets:select(S, [ {  {{'$1',{'$2','$3','$4'}},'_',PID},
						     [{ 'and', 
						       {'=/=','$1',SID},
						       {'>=',
							{'+',{'*',{'+',{'*','$2',1000000},'$3'},1000000},'$4'},
							{'+',{'*',{'+',{'*',Tin1,1000000},Tin2},1000000},Tin3}},

						       {'<',
							{'+',{'*',{'+',{'*','$2',1000000},'$3'},1000000},'$4'},
							{'+',{'*',{'+',{'*',Tout1,1000000},Tout2},1000000},Tout3}}}],
						     ['$_']}]) of
				[] -> ok;
				L-> 			    
				    io:write({ {SID,{Tin1,Tin2,Tin3}}, {Tout1,Tout2,Tout3},PID}),
				    io:write('        '),
				    io:write(L),io:nl(),io:nl()
			    end,
			    continue
		    end).


sd()->
    S=t(),
    lists:map(fun(Zoom)->
       lists:map(fun(Key)->
			 case
			     {dets:match(S,{{1,Zoom,Key},'$1'}),dets:match(S,{{2,Zoom,Key},'$1'})}
			 of
			     {[[V1]],[[V2]]}->
				 case overlap(V1,V2) of
				     false -> ok;
				     true -> 
					 io:write({Zoom,Key}),io:nl(),
					 %% io:write(V1),io:nl(),
					 %% io:write(V2),io:nl(),
					 io:nl()


				 end;
			     _-> ok
			 end
		 end,lists:seq(1,257))
	      end,lists:seq(0,20)).


overlap([],[])->
    false;
overlap([],_)->
    false;
overlap(_,[])->
    false;
overlap([V1|T1],[V2|T2])->
    if V1+V2 > 8 ->
	    true;
       true->
	    overlap(T1,T2)
    end.
    
