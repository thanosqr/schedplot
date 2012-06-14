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
%%     if X div 100 == 0 ->
%% %	    herp(1000000),
%% 	    qijap:print(0);
%%        true -> ok
%%     end,
    derp(X-1).


herp(0)->
    ok;
herp(X) ->
    herp(X-1).
