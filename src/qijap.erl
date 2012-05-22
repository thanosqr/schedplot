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

start(M,F,Args)->
	pcore:start(M,F,Args).

start({M,F,Args})->
	start({M,F,Args},[]).
start({M,F,Args},Flags)->
	start(M,F,Args,erlang:system_info(schedulers),Flags).

start(M,F,Args,FolderName,CoreN,Flags)->
	pcore:start(M,F,Args,FolderName,CoreN,Flags).

start(M,F,Args,CoreN,Flags)->
	start(M,F,Args,?DEFAULT_FOLDER_NAME,CoreN,Flags).

stop()->
	pcore:stop().

start()->
	start({timer_wheel,wheel,[500]}).

tw(X)->
	start({timer_wheel,wheel,[X]}).
