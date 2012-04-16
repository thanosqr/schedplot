-module(qijap).
-compile(export_all).

-define(DEFAULT_FNAME,trace_famdict).
-define(DEFAULT_GNAME,trace_gabi).
-define(DEFAULT_TNAME,analyzed_trace).

view(Filename)->
		uds:init(Filename).

view()->
		view(?DEFAULT_TNAME).

analyze(In, Out)->
		ibap:analyze(In,Out).

analyze()->
		analyze(?DEFAULT_GNAME,?DEFAULT_TNAME).

analyze(CoreN)->
    ibap:analyze(?DEFAULT_GNAME,?DEFAULT_TNAME,8,CoreN).

start(M,F,Args)->
		pcore:start(M,F,Args).

start({M,F,Args})->
		start({M,F,Args},[]).
start({M,F,Args},Flags)->
		start(M,F,Args,erlang:system_info(schedulers),Flags).

start(M,F,Args,GName,FName,CoreN,Flags)->
		pcore:start(M,F,Args,GName,FName,CoreN,Flags).

start(M,F,Args,CoreN,Flags)->
		start(M,F,Args,?DEFAULT_GNAME,?DEFAULT_FNAME,CoreN,Flags).

stop()->
		pcore:stop().

start()->
		start({timer_wheel,wheel,[500]}).
