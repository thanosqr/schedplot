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

%% tw(X)->
%% 	start({timer_wheel,wheel,[X]}).

start(Fun,FolderName,Flags)->
	pcore:start(Fun,FolderName,erlang:system_info(schedulers),Flags).

start(Fun,Flags) when erlang:is_list(Flags) ->
	start(Fun,?DEFAULT_FOLDER_NAME,Flags);
start(Fun,FolderName) ->
	start(Fun,FolderName,[]).

start(Fun)->
	start(Fun,?DEFAULT_FOLDER_NAME,[]).
