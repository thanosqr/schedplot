-module(qijap).

-export([start/1, start/2, start/3, stop/0,
	 analyze/0, analyze/1, analyze/2,
	 view/0, view/1, print/1]).

-export_type([folder/0, label/0, start_flags/0]).

-type folder()      :: file:filename() | atom() | integer().
-type label()       :: string() | atom() | integer().
-type anal_flags()  :: []. %% TO BE REVISED
-type start_flags() :: [start_flag()].

-type start_flag() :: 'gc' | 'no_auto_stop' | 'trace_all' | 'trace_mfa'.

-include("hijap.hrl").

-define(AN_FLAGS, []).
-define(AN_CONFL, []).
-define(START_FLAGS, [gc, no_auto_stop, trace_all, trace_mfa]).

-spec start(function()) -> 'ok'.
start(Fun) ->
    start(Fun, ?DEFAULT_FOLDER_NAME, []).

-spec start(function(), folder()) -> 'ok'.
start(Fun, FolderName) ->
    start(Fun, FolderName, []).

-spec start(function(), folder(), start_flags()) -> 'ok'.
start(Fun, FolderName, Flags) ->
    ok = check_flags(Flags, ?START_FLAGS),
    N = case lists:member(gc, Flags) of
	    false -> erlang:system_info(schedulers);
	    true  -> erlang:system_info(schedulers) + 1
	end,
    pcore:start(Fun, ensure_string(FolderName), N, Flags).

-spec stop() -> 'ok'.
stop() ->
    pcore:stop().

-spec analyze() -> 'ok'.
analyze() ->
    analyze(?DEFAULT_FOLDER_NAME, []).

-spec analyze(folder()) -> 'ok'.
analyze(FolderName) ->
    analyze(FolderName, []).

-spec analyze(folder(), anal_flags()) -> 'ok'.
analyze(FolderName, Flags) ->
    SFolderName = ensure_string(FodlerName),
    HName = SFolderName ++ "/trace_gabi_header",
    {ok, F} = file:open(HName, [read]),
    {ok, CoreN} = io:read(F, ''),
    _ = io:read(F, ''),
    {ok, FullPacket} = io:read(F, ''),
    Mode = case FullPacket of
               true -> 'full_packet';
               false -> 'time_packet'
           end,
    ok = file:close(F),
    ok = check_flags(Flags, ?AN_FLAGS),
    ok = conflicting_flags(Flags, ?AN_CONFL),    
    NGU = case lists:keyfind(group, 1, Flags) of
	      false -> ?GU;
	      {group, GU} -> GU
	  end,
    ibap:analyze(SFolderName, NGU, {1, CoreN}, Mode).

-spec view() -> 'ok'.
view() ->
    view(?DEFAULT_FOLDER_NAME).

-spec view(folder()) -> 'ok'.
view(FolderName) ->
    viewer:start(ensure_string(FolderName)).

-spec print(label()) -> 'ok'.
print(Label) ->
    scarlet:print(ensure_string(Label)).

ensure_string(Name) when is_atom(Name) ->
    atom_to_list(Name);
ensure_string(Name) when is_integer(Name) ->
    integer_to_list(Name);
ensure_string(Name) when is_list(Name) ->
    Name;
ensure_string(_) -> 
    {error, expecting_atom_integer_or_string}.



%%----------------------------------------------------------%%	
%%--------------------- local functions --------------------%%
%%----------------------------------------------------------%%	

check_flags(Flags, Valid) ->
    case [F || F <- Flags, not lists:member(F, Valid)] of
	[] -> ok;
	Inv ->
	    io:format("Warning: invalid flag(s): ~w\n", [Inv])
    end.

conflicting_flags(_Flags, _Confls) ->
    %% case [C || C <- Confl, lists:member(C, Flags)] of
    %%	[] -> ok
    %%  [_] -> ok;
    %%  [H|_] = L ->
    %%     io:format("Warning: conflicting flags: ~w, using ~w\n", [L, H])
    %% end.
    ok.

%%----------------------------------------------------------%%	
%%-------------------------- testing -----------------------%%
%%----------------------------------------------------------%%	

-ifdef(TEST).

list(X) ->
    start({lists,seq,[1,X*1000*100]},[trace_all]).

nlist(X) ->
    start({lists,seq,[1,X*1000*100]}).

t() ->
    {ok,S} = dets:open_file("qijap_profile/analyzed_trace"),
    S.

r(S, X) ->
    lists:reverse(lists:sort(dets:match(S,{{X,0,'$1'},'_'}))).

seqs(X) ->
    start({?MODULE,herps,[X*1000*1000]},[]).

herps(X) ->
    spawn(?MODULE,herp,[X,self()]),
    receive
	done -> ok
    end.

herp(X, P) ->
    derps(X),
    P ! done.

seq(X) ->
    start({?MODULE,derps,[X*1000*1000]}).

derps(X) ->
    %% qijap:print("start"),
    derp(X).
derp(0) ->
    %% qijap:print("end"),
    ok;
derp(X) ->
    N = 1000*1000*2,
    if X rem N =:= 0 ->
   	    qijap:print(integer_to_list(X div N));
       true -> ok
    end,
    derp(X-1).

herp(0) ->
    ok;
herp(X) ->
    herp(X-1).

%% { {PID = SID, TimeIn}, TimeOut,PID}
search() ->
    {ok,S} = dets:open_file("saved/p220",[]),
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


sd(C1,C2)->
    S = t(),
    lists:map(fun(Zoom) ->
		      lists:map(fun(Key) ->
					case
					    {dets:match(S,{{C1,Zoom,Key},'$1'}),dets:match(S,{{C2,Zoom,Key},'$1'})}
					of
					    {[[V1]],[[V2]]}->
						case overlap(V1,V2) of
						    0 -> ok;
						    N -> 
							io:write({Zoom,Key,N}),io:nl(),
							io:write(V1),io:nl(),
							io:write(V2),io:nl(),
							io:nl()


						end;
					    _-> ok
					end
				end,lists:seq(1,500))
	      end,lists:seq(0,0)).

overlap([], _) ->
    0;
overlap(_, []) ->
    0;
overlap([V1|T1], [V2|T2]) ->
    if V1+V2 > 8 ->
	    overlap(T1, T2) + 1;
       true ->
	    overlap(T1, T2)
    end.

-endif.
