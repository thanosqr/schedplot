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

 
	
